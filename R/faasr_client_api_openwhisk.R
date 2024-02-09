#' @title Client tool for registering openwhisk actions.
#' @description faasr_register_workflow_ibmcloud_openwhisk: 
#' starts to register functions in the JSON file for OpenWhisk
#' @description faasr_register_workflow_ibmcloud_action_lists: 
#' creates a list of Server and Action name pairs               
#' @description faasr_register_workflow_ibmcloud_target_group: 
#' targets a group: ibmcloud openwhisk needs to target group/default is "Default"             
#' @description faasr_register_workflow_ibmcloud_create_namespace: 
#' creates a new namespace if given namespace doesn't exist  
#' @description faasr_register_workflow_ibmcloud_target_namespace: 
#' targets a namespace with the given namespace              
#' @description faasr_register_workflow_ibmcloud_create_action:
#' registers the functions with ibmcloud API              
#'
#' @param faasr_register_workflow_ibmcloud_openwhisk: 
#' "faasr" for list of the json, "cred" for the list of credentials       
#' @param faasr_register_workflow_ibmcloud_action_lists:
#' "faasr" for list of the json
#' @param faasr_register_workflow_ibmcloud_create_namespace:
#' "faasr" for list of the json 
#' @param faasr_register_workflow_ibmcloud_target_namespace: 
#' "name" for the string
#' @param faasr_register_workflow_ibmcloud_create_action:
#' "actionname" for the string, "faasr" for list of the json 
#' 
#' @return faasr_register_workflow_ibmcloud_openwhisk: 
#' "faasr" for list of the json
#' @return faasr_register_workflow_ibmcloud_action_lists: 
#' "action_list" for the list of servername:actionname pairs
#' @return faasr_register_workflow_ibmcloud_create_namespace:
#' "name_id" for the string of namespace id

faasr_register_workflow_openwhisk <- function(faasr, cred, ssl=TRUE, memory=1024, timeout=600000) {
  
  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")

  # create a server-action set
  action_list <- faasr_register_workflow_openwhisk_action_lists(faasr)
  
  if (length(action_list)==0){
    return("")
  }

  # check servers and actions, create actions
  for (server in names(action_list)) {

    cli::cli_h1(paste0("Registering workflow for openwhisk: ", server))
    cli::cli_progress_bar(
      format = paste0(
        "FaaSr {pb_spin} Registering workflow openwhisk ",
        "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
      ),
      format_done = paste0(
        "{col_yellow(symbol$checkbox_on)} Successfully registered actions for server {server} ",
        "in {pb_elapsed}."
      ),
      total = length(action_list[[server]]) * 2
    )

    faasr <- faasr_replace_values(faasr, cred)

    for (act in action_list[[server]]) {
      action <- paste0("actions/", act)
      check <- faasr_register_workflow_openwhisk_check_exists(ssl, action, server, faasr)
      cli_progress_update()
      faasr_register_workflow_openwhisk_create_action(ssl, act, server, faasr,  memory, timeout, check)
      cli_progress_update()
    }
  }
  cli_text(col_cyan("{symbol$menu} {.strong Successfully registered all openwhisk actions}"))
}

# help sending httr requests
faasr_ow_httr_request <- function(faasr, server, action, type, body=list(), ssl=TRUE, namespace=NULL){

  library("httr")

  endpoint <- faasr$ComputeServers[[server]]$Endpoint
  if (!startsWith(endpoint, "https://")){
    endpoint <- paste0("https://", endpoint)
  }
  if (is.null(namespace)){
    namespace <- faasr$ComputeServers[[server]]$Namespace
  }

  if (!is.null(faasr$ComputeServers[[server]]$SSL) && length(faasr$ComputeServers[[server]]$SSL)!=0){
      ssl <- as.logical(toupper(faasr$ComputeServers[[server]]$SSL))
  }
  
  api_key <- faasr$ComputeServers[[server]]$API.key

  # get functions depending on "type"
  func <- get(type)
  api_key <- strsplit(api_key, ":")[[1]]

  # write headers
  headers <- c(
    'accept' = 'application/json', 
    'Content-Type' = 'application/json'
  )

  # send the REST request(POST/GET/PUT/PATCH)
  response <- func(
    url = paste0(endpoint, "/api/v1/namespaces/", namespace, "/", action),
    authenticate(api_key[1], api_key[2]),
    add_headers(.headers = headers),
    body=body,
    encode="json",
    httr::config(ssl_verifypeer = ssl, ssl_verifyhost = ssl),
    accept_json()
  )

  return(response)
}


faasr_register_workflow_openwhisk_action_lists <- function(faasr) {
  # empty list
  action_list <- list()
  # for each function, iteratively collect server names and action names
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is Openwhisk, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      err_msg <- paste0("Invalid server:", server_name," check server type")
      cli_alert_danger(err_msg)
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "OpenWhisk") {
      action_name <- fn
      action_list[[server_name]] <- unique(c(action_list[[server_name]],action_name))
    }
  }
  return(action_list)
}

faasr_register_workflow_openwhisk_check_exists <- function(ssl, action, server, faasr){
  
  response <- faasr_ow_httr_request(faasr, server, action, type="GET", ssl=ssl)
  
  ######### NEED TO BE SPECIFIED
  if (response$status_code==200){
    succ_msg <- paste0("Check ",action," exists: TRUE - Found")
    cli_alert_warning(succ_msg)
    return(TRUE)
  } else if (response$status_code==404){
    alert_msg <- paste0("Check ",action," exists: FALSE - Create New")
    cli_alert_success(alert_msg)
    return(FALSE)
  } else {
    err_msg <- paste0("Check ",action," exists Error: ", content(response)$error)
    cli_alert_danger(err_msg)
    stop()
  }
}

faasr_register_workflow_openwhisk_check_user_input <- function(check, actionname, type){
  # if given values already exists, ask the user to update the action
  if (check){
    cli_alert_info(paste0("Do you want to update the ",type,"?[y/n]"))

    while(TRUE) {
      check <- readline()
      if (check=="y") {
        overwrite <- "true"
        break
      } else if(check=="n") {
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
      }
    }
  } else {
    overwrite <- "false"
  }
  return(overwrite)
}

# create an action
faasr_register_workflow_openwhisk_create_action <- function(ssl, actionname, server, faasr, memory, timeout, check) {
  
  overwrite <- faasr_register_workflow_openwhisk_check_user_input(check, actionname, type="action")
  if (overwrite == "true"){
    action_performed <- "Update"
  } else{
    action_performed <- "Create"
  }

  # actioncontainer can be either default or user-customized
  if (length(faasr$ActionContainers[[actionname]])==0 || faasr$ActionContainers[[actionname]] == "") {
    actioncontainer <- "faasr/openwhisk-tidyverse:latest"
  } else {
    actioncontainer <- faasr$ActionContainers[[actionname]]
  }
  # create a function with maximum timeout and 512MB memory space

  body <- list(
    exec = list(
      kind = "blackbox",
      image = actioncontainer
    ),
    limits = list(
      timeout = as.numeric(timeout),
      memory = as.numeric(memory)
    )
  )

  action <- paste0("actions/", actionname, "?overwrite=", overwrite)
  response <- faasr_ow_httr_request(faasr, server, action, type="PUT", body=body, ssl)
  if (response$status_code==200 || response$status_code==202){
    succ_msg <- paste0("Successfully ", action_performed," the function - ", actionname)
    cli_alert_success(succ_msg)
  } else {
    err_msg <- paste0("Error  ", action_performed," the function - ", actionname,": ",content(response)$error)
    cli_alert_danger(err_msg)
    stop()
  }
  
}

faasr_workflow_invoke_openwhisk <- function(faasr, cred, faas_name, actionname, ssl=TRUE){

  action <- paste0("actions/", actionname, "?blocking=false&result=false")
  faasr <- faasr_replace_values(faasr, cred)
  body <- faasr
  response <- faasr_ow_httr_request(faasr, faas_name, action, type="POST", body=body, ssl)
  if (response$status_code==200 || response$status_code==202){
    succ_msg <- paste0("Successfully invoke the function - ", actionname, ", activation ID: ", content(response)$activationId)
    cli_alert_success(succ_msg)
  } else {
    err_msg <- paste0("Error invoke the function - ", actionname,": ",content(response)$error)
    cli_alert_danger(err_msg)
    stop()
  }

}

# set workflow timer for openwhisk
faasr_set_workflow_timer_ow <- function(faasr, cred, target, cron, unset=FALSE, ssl=TRUE){

  # set variables
  server <- faasr$FunctionList[[target]]$FaaSServer
  trigger_name <- paste0(target,"_trigger")
  rule_name <- paste0(target,"_rule")
  api_key <- faasr$ComputeServers[[server]]$API.key
  namespace <- faasr$ComputeServers[[server]]$Namespace

  # json should get out two layers, so escaping letter should be twice
  faasr <- faasr_replace_values(faasr, cred)
   
  # if unset==TRUE, delete the rule and trigger
  if (unset==TRUE){
    action <- paste0("triggers/", trigger_name) 
    check <- faasr_register_workflow_openwhisk_check_exists(ssl, action, server,faasr)
    
    overwrite <- faasr_register_workflow_openwhisk_check_user_input(check, trigger_name, type="trigger")
    if (overwrite == "true"){
      action_performed <- "Create"
    } else{
      action_performed <- "Update"
    }

    action <- paste0(action, "?overwrite=",overwrite)
    response <- faasr_ow_httr_request(faasr, server, action, type="PUT", ssl)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully ", action_performed," the trigger - ", trigger_name)
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error  ", action_performed," the trigger - ", trigger_name,": ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }

    ## fire the alarm
    namespace_system <- "whisk.system"
    action <- paste0("actions/alarms/alarm?blocking=false&result=false")
    body <- list(
      authKey = api_key,
      cron = cron,
      trigger_payload = faasr,
      lifecycleEvent = "CREATE",
      triggerName = trigger_name
    )
    response <- faasr_ow_httr_request(faasr, server, action, type="POST", body=body, ssl, namespace=namespace_system)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully fire the alarm")
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error fire the alarm: ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }

    # check the rule
    action <- paste0("rules/", rule_name) 
    check <- faasr_register_workflow_openwhisk_check_exists(ssl, action, server,faasr)
    
    overwrite <- "true"
    
    # create the rule
    action <- paste0(action, "?overwrite=",overwrite)
    body <- list(
      name = rule_name,
      status = "",
      trigger = paste0("/", namespace, "/", trigger_name),
      action = paste0("/", namespace, "/", target)
    )
    response <- faasr_ow_httr_request(faasr, server, action, type="PUT", body=body, ssl)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully ", action_performed," the rule - ", rule_name)
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error  ", action_performed," the rule - ", rule_name,": ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }


  # if unset=FALSE, set the rule and trigger
  } else {
    
    action <- paste0("triggers/", trigger_name) 
    check <- faasr_register_workflow_openwhisk_check_exists(ssl, action, server,faasr)
    if (!check){
      err_msg <- paste0("Error: No ",trigger_name," found")
      cli_alert_danger(err_msg)
      stop()
    }
    
    ## stop the alarm
    namespace <- "whisk.system"
    action <- paste0("actions/alarms/alarm?blocking=false&result=false")
    body <- list(
      authKey = api_key,
      lifecycleEvent = "DELETE",
      triggerName = trigger_name
    )
    response <- faasr_ow_httr_request(faasr, server, action, type="POST", body=body, ssl, namespace=namespace)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully Stop the alarm")
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error Stop the alarm: ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }

    # delete the trigger
    action <- paste0("triggers/", trigger_name) 
    response <- faasr_ow_httr_request(faasr, server, action, type="DELETE", ssl)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully Delete the trigger - ", trigger_name)
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error Delete the trigger - ", trigger_name,": ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }


    # check the rule
    action <- paste0("rules/", rule_name) 
    check <- faasr_register_workflow_openwhisk_check_exists(ssl, action, server,faasr)
    if (!check){
      err_msg <- paste0("Error: No ",rule_name," found")
      cli_alert_danger(err_msg)
      stop()
    }
    
    # disable the rule
    action <- paste0(action, "?overwrite=true")
    body <- list(
      status = "inactive",
      trigger = "null",
      action = "null"
    )
    response <- faasr_ow_httr_request(faasr, server, action, type="POST", body=body, ssl)
    ####response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully Delete the rule - ", rule_name)
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error Delete the rule - ", rule_name,": ",content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }
  }
}
