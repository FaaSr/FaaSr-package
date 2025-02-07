#' @name faasr_register_workflow_google_cloud
#' @title faasr_register_workflow_google_cloud
#' @description 
#' register the workflow for google cloud.
#' parse faasr to get the server list and actions.
#' create a actions for the FaaSr actions.
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param memory an integer for the max size of memory
#' @param timeout an integer for the max length of timeout
#' @import httr
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud <- function(faasr, cred, ssl=TRUE, memory=1024, timeout=86400, storage=NULL) {
  
  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")

  # create a server-action set
  action_list <- faasr_register_workflow_google_cloud_action_lists(faasr)

  if (length(action_list)==0){
    return("")
  }

  # check servers and actions, create actions
  for (server in names(action_list)) {

    cli::cli_h1(paste0("Registering workflow for google cloud: ", server))
    cli::cli_progress_bar(
      format = paste0(
        "FaaSr {pb_spin} Registering workflow google cloud ",
        "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
      ),
      format_done = paste0(
        "{col_yellow(symbol$checkbox_on)} Successfully registered actions for server {server} ",
        "in {pb_elapsed}."
      ),
      total = length(action_list[[server]]) * 2 + 1
    )

    faasr <- faasr_replace_values(faasr, cred)

    response <- faasr_gcp_httr_request(faasr, server, "/jobs", type="GET", ssl=ssl)

    if (response$status_code==200 || response$status_code==202){
        all_gcloud_jobs <- content(response)$jobs
        succ_msg <- paste0("All exising gcloud jobs fetched: ", length(content(response)$jobs), " jobs")
        cli_alert_success(succ_msg)
        cli_progress_update()
    } else {
        err_msg <- paste0("Error fetching all exising gcloud jobs: ", content(response)$error)
        cli_alert_danger(err_msg)
        stop()
    }

    for (action in action_list[[server]]) {
      check <- faasr_register_workflow_google_cloud_check_exists(action, server, faasr, all_gcloud_jobs)
      cli_progress_update()
      faasr_register_workflow_google_cloud_create_action(ssl, action, server, faasr,  memory, timeout, check)
      cli_progress_update()
    }
  }
  cli_text(col_cyan("{symbol$menu} {.strong Successfully registered all google cloud actions}"))
}

#' @title faasr_gcp_httr_request
#' @description 
#' the help function to send the curl request to the google cloud
#' by using the "httr" library. 
#' @param faasr a list form of the JSON file
#' @param server a string for the target server
#' @param action a string for the target action: /actions, /triggers, /rules
#' @param type REST API values; GET/PUT/DELETE/PATCH/POST
#' @param body a list of body
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param namespace a string for the specific namespace e.g., /whisk.system
#' @return an integer value for the response
#' @import httr
#' @import cli
#' @keywords internal

# help sending httr requests
faasr_gcp_httr_request <- function(faasr, server, action, type, body=NULL, ssl=TRUE, namespace=NULL){

  endpoint <- faasr$ComputeServers[[server]]$Endpoint
  if (!startsWith(endpoint, "https://")){
    endpoint <- paste0("https://", endpoint)
  }
  if (is.null(namespace)){
    namespace <- faasr$ComputeServers[[server]]$Namespace
  }
  region <- faasr$ComputeServers[[server]]$Region
  endpoint <- paste0(endpoint, namespace, "/locations/", region, action)

  if (!is.null(faasr$ComputeServers[[server]]$SSL) && length(faasr$ComputeServers[[server]]$SSL)!=0){
      ssl <- as.logical(toupper(faasr$ComputeServers[[server]]$SSL))
  }
  
  token <- faasr$ComputeServers[[server]]$AccessKey

  # get functions depending on "type"
  func <- get(type)

  # write headers
  headers <- c(
    'accept' = 'application/json',
    'Content-Type' = 'application/json',
    'Authorization' = paste("Bearer", token)
  )

  print(paste("Sending request to", endpoint))
  # send the REST request (POST/GET/DELETE) with timeout
  response <- func(
    url = endpoint,
    add_headers(.headers = headers),
    body = body,
    encode = "json",
    httr::config(ssl_verifypeer = ssl, ssl_verifyhost = ssl),
    accept_json()
  )

  return(response)
}


#' @title faasr_register_workflow_google_cloud_action_lists
#' @description 
#' Parse the faasr and get the list of function:server
#' Find actions which is using "GoogleCloud"
#' return value's key is action and value is server name.
#' @param faasr a list form of the JSON file
#' @return an list of "action name: server name" pairs
#' @import httr
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_action_lists <- function(faasr) {
  # empty list
  action_list <- list()
  # for each function, iteratively collect server names and action names
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is GoogleCloud, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      err_msg <- paste0("Invalid server:", server_name," check server type")
      cli_alert_danger(err_msg)
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "GoogleCloud") {
      action_name <- fn
      action_list[[server_name]] <- unique(c(action_list[[server_name]],action_name))
    }
  }
  return(action_list)
}


#' @title faasr_register_workflow_google_cloud_check_exists
#' @description 
#' Check the remote repository is existing on the google cloud
#' by sending the GET request.
#' If it exists, return TRUE, doesn't exist, return FALSE
#' @param action a string for the target action: /actions, /triggers, /rules
#' @param server a string for the target server
#' @param faasr a list form of the JSON file
#' @param all_gcloud_jobs a list of all existing gcloud jobs
#' @return a logical value; if exists, return TRUE, 
#' doesn't exist, return FALSE
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_check_exists <- function(action, server, faasr, all_gcloud_jobs){

  current_job_name <- paste0("projects/", faasr$ComputeServers[[server]]$Namespace, "/locations/", faasr$ComputeServers[[server]]$Region, "/jobs/", action)
  for(job in all_gcloud_jobs)
  {
    if(job$name == current_job_name)
    {
      succ_msg <- paste0("Check ", action, " exists: TRUE - Found")
      cli_alert_warning(succ_msg)
      return(TRUE)
    }
  }
  alert_msg <- paste0("Check ", action, " exists: FALSE - Create New")
  cli_alert_success(alert_msg)
  return(FALSE)
}


#' @title faasr_register_workflow_google_cloud_check_delete_job_user_input
#' @description 
#' Ask user input for the google cloud
#' @param check a logical value for target existence
#' @param actionname a string for the target action name
#' @return a logical value for delete
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_check_delete_job_user_input <- function(check, actionname){
  # if given values already exists, ask the user to update the action
  delete_job <- FALSE
  if (check){
    cli_alert_info(paste0("Do you want to delete and create the job?[y/n]"))

    while(TRUE) {
      check <- readline()
      if (check=="y" || check=="") {
        delete_job <- TRUE
        break
      } else if(check=="n") {
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
      }
    }
  }
  return(delete_job)
}


#' @title faasr_register_workflow_google_cloud_check_create_job_user_input
#' @description 
#' Ask user input for the google cloud
#' @param actionname a string for the target action name
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_check_create_job_user_input <- function(actionname){
  # if given values already exists, ask the user to update the action
    cli_alert_info(paste0("Proceed to create the job - ", actionname, "?[y/n]"))

    while(TRUE) {
      check <- readline()
      if (check=="y" || check=="") {
        break
      } else if(check=="n") {
        err_msg <- paste0("Google cloud job creation cancelled for the function - ", actionname)
        cli_alert_danger(err_msg)
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
      }
    }
}

#' @title faasr_register_workflow_google_cloud_create_action
#' @description 
#' Create an action
#' if it already exists and user wants, update the action
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param actionname a string for the target action name
#' @param server a string for the target server
#' @param faasr a list form of the JSON file
#' @param memory an integer for the max size of memory
#' @param timeout an integer for the max length of timeout
#' @param check a logical value for target existence
#' @import httr
#' @import cli
#' @keywords internal

# create an action
faasr_register_workflow_google_cloud_create_action <- function(ssl, actionname, server, faasr, memory, timeout, check) {
  
  delete_job <- faasr_register_workflow_google_cloud_check_delete_job_user_input(check, actionname)
  if (delete_job){
    # delete the gcloud job
    action <- paste0("/jobs/", actionname)
    response <- faasr_gcp_httr_request(faasr, server, action, type="DELETE")
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully deleted gcloud job for the function - ", actionname)
      cli_alert_success(succ_msg)
      faasr_register_workflow_google_cloud_check_create_job_user_input(actionname)
    } else {
      err_msg <- paste0("Error deleting gcloud job for the function - ", actionname, ": ", content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }
  }

  # actioncontainer can be either default or user-customized
  if (length(faasr$ActionContainers[[actionname]])==0 || faasr$ActionContainers[[actionname]] == "") {
    actioncontainer <- "gcr.io/faasr/google-cloud-tidyverse:latest"
  } else {
    actioncontainer <- faasr$ActionContainers[[actionname]]
  }
  # create a function with maximum timeout and 512MB memory space

  body <- list(
    template = list(
      template = list(
        containers = list(
          list(
            image = actioncontainer,
            resources = list(
              limits = list(
                memory = paste0(memory, "Mi")
              )
            )
          )
        ),
        timeout = paste0(timeout, "s") # Set service timeout (e.g., "24 * 60 * 60 s" for 24 hours)
      )
    )
  )

  action <- paste0("/jobs?jobId=", actionname)
  response <- faasr_gcp_httr_request(faasr, server, action, type="POST", body=body, ssl)
  if (response$status_code==200 || response$status_code==202){
    succ_msg <- paste0("Successfully created gcloud job for the function - ", actionname)
    cli_alert_success(succ_msg)
  } else {
    err_msg <- paste0("Error in creating gcloud job for the function - ", actionname, ": ", content(response)$error)
    cli_alert_danger(err_msg)
    stop()
  }
  
}


#' @title faasr_workflow_invoke_google_cloud
#' @description 
#' Invoke a workflow for the google cloud
#' this function is invoked by faasr_workflow_invoke
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param faas_name a string for the target server
#' @param actionname a string for the target action name
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @import httr
#' @import cli
#' @keywords internal

faasr_workflow_invoke_google_cloud <- function(faasr, cred, faas_name, actionname, ssl=TRUE){

  action <- paste0("/jobs/", actionname, ":run")
  faasr <- faasr_replace_values(faasr, cred)

  # Prepare the args from the faasr object
  args <- jsonlite::toJSON(faasr, auto_unbox = TRUE)

  # Build the body for the HTTP request without specifying the image
  body <- list(
    overrides = list(
      containerOverrides = list(
        list(
          args = args
        )
      )
      # taskCount = 1, # Configures as per need while invoking the job
      # timeout = paste0(service_timeout_seconds, "s")  # Set service timeout
    )
  )
  
  response <- faasr_gcp_httr_request(faasr, faas_name, action, type="POST", body=body, ssl)
  if (response$status_code==200 || response$status_code==202){
    succ_msg <- paste0("Successfully invoke the function - ", actionname)
    cli_alert_success(succ_msg)
  } else {
    err_msg <- paste0("Error invoke the function - ", actionname, ": ", content(response)$error)
    cli_alert_danger(err_msg)
    stop()
  }

}


# #' @title faasr_set_workflow_timer_google_cloud
# #' @description 
# #' # set/unset workflow cron timer for google cloud
# #' @param faasr a list form of the JSON file
# #' @param cred a list form of the credentials
# #' @param target a string for the target action
# #' @param cron a string for cron data e.g., */5 * * * *
# #' @param unset a logical value; set timer(FALSE) or unset timer(TRUE)
# #' @param ssl SSL CA check; for the SSL certificate: FALSE
# #' @import httr
# #' @import cli
# #' @keywords internal

# # set workflow timer for google cloud
# faasr_set_workflow_timer_gcp <- function(faasr, cred, target, cron, unset=FALSE, ssl=TRUE){

#   # set variables
#   server <- faasr$FunctionList[[target]]$FaaSServer
#   trigger_name <- paste0(target,"_trigger")
#   rule_name <- paste0(target,"_rule")
#   api_key <- faasr$ComputeServers[[server]]$API.key
#   namespace <- faasr$ComputeServers[[server]]$Namespace

#   # json should get out two layers, so escaping letter should be twice
#   faasr <- faasr_replace_values(faasr, cred)
   
#   # if unset==TRUE, delete the rule and trigger
#   if (unset==TRUE){
#     action <- paste0("triggers/", trigger_name) 
#     check <- faasr_register_workflow_google_cloud_check_exists(ssl, action, server,faasr)
    
#     overwrite <- faasr_register_workflow_google_cloud_check_user_input(check, trigger_name, type="trigger")
#     if (overwrite == "true"){
#       action_performed <- "Create"
#     } else{
#       action_performed <- "Update"
#     }

#     action <- paste0(action, "?overwrite=",overwrite)
#     response <- faasr_gcp_httr_request(faasr, server, action, type="PUT", ssl)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully ", action_performed," the trigger - ", trigger_name)
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error  ", action_performed," the trigger - ", trigger_name,": ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }

#     ## fire the alarm
#     namespace_system <- "whisk.system"
#     action <- paste0("actions/alarms/alarm?blocking=false&result=false")
#     body <- list(
#       authKey = api_key,
#       cron = cron,
#       trigger_payload = faasr,
#       lifecycleEvent = "CREATE",
#       triggerName = trigger_name
#     )
#     response <- faasr_gcp_httr_request(faasr, server, action, type="POST", body=body, ssl, namespace=namespace_system)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully fire the alarm")
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error fire the alarm: ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }

#     # check the rule
#     action <- paste0("rules/", rule_name) 
#     check <- faasr_register_workflow_google_cloud_check_exists(ssl, action, server,faasr)
    
#     overwrite <- "true"
    
#     # create the rule
#     action <- paste0(action, "?overwrite=",overwrite)
#     body <- list(
#       name = rule_name,
#       status = "",
#       trigger = paste0("/", namespace, "/", trigger_name),
#       action = paste0("/", namespace, "/", target)
#     )
#     response <- faasr_gcp_httr_request(faasr, server, action, type="PUT", body=body, ssl)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully ", action_performed," the rule - ", rule_name)
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error  ", action_performed," the rule - ", rule_name,": ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }


#   # if unset=FALSE, set the rule and trigger
#   } else {
    
#     action <- paste0("triggers/", trigger_name) 
#     check <- faasr_register_workflow_google_cloud_check_exists(ssl, action, server,faasr)
#     if (!check){
#       err_msg <- paste0("Error: No ",trigger_name," found")
#       cli_alert_danger(err_msg)
#       stop()
#     }
    
#     ## stop the alarm
#     namespace <- "whisk.system"
#     action <- paste0("actions/alarms/alarm?blocking=false&result=false")
#     body <- list(
#       authKey = api_key,
#       lifecycleEvent = "DELETE",
#       triggerName = trigger_name
#     )
#     response <- faasr_gcp_httr_request(faasr, server, action, type="POST", body=body, ssl, namespace=namespace)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully Stop the alarm")
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error Stop the alarm: ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }

#     # delete the trigger
#     action <- paste0("triggers/", trigger_name) 
#     response <- faasr_gcp_httr_request(faasr, server, action, type="DELETE", ssl)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully Delete the trigger - ", trigger_name)
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error Delete the trigger - ", trigger_name,": ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }


#     # check the rule
#     action <- paste0("rules/", rule_name) 
#     check <- faasr_register_workflow_google_cloud_check_exists(ssl, action, server,faasr)
#     if (!check){
#       err_msg <- paste0("Error: No ",rule_name," found")
#       cli_alert_danger(err_msg)
#       stop()
#     }
    
#     # disable the rule
#     action <- paste0(action, "?overwrite=true")
#     body <- list(
#       status = "inactive",
#       trigger = "null",
#       action = "null"
#     )
#     response <- faasr_gcp_httr_request(faasr, server, action, type="POST", body=body, ssl)
#     ####response handling: status code
#     if (response$status_code==200 || response$status_code==202){
#       succ_msg <- paste0("Successfully Delete the rule - ", rule_name)
#       cli_alert_success(succ_msg)
#     } else {
#       err_msg <- paste0("Error Delete the rule - ", rule_name,": ",content(response)$error)
#       cli_alert_danger(err_msg)
#       stop()
#     }
#   }
# }
