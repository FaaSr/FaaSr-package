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
    faasr <- faasr_refresh_gcp_accesskey(faasr, server)

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

#' @title faasr_refresh_gcp_accesskey
#' @description 
#' function to get a new access using the JWT token
#' @param faasr a list form of the JSON file
#' @param server a string for the target server
#' @return response faasr object with updated access key for gcp
#' @import httr
#' @import cli
#' @import jsonlite
#' @import openssl
#' @keywords internal

faasr_refresh_gcp_accesskey <- function(faasr, server){

  client_email <- faasr$ComputeServers[[server]]$ClientEmail
  private_key  <- faasr$ComputeServers[[server]]$SecretKey
  token_uri    <- faasr$ComputeServers[[server]]$TokenUri

  # Create JWT
  # Helper: Base64 URL-safe encoding
  base64url_encode <- function(x) {
    gsub("=+$", "", gsub("\\+", "-", gsub("/", "_", base64_enc(x))))
  }

  # JWT header and payload (claims)
  header <- list(alg = "RS256", typ = "JWT")
  issued_at <- as.integer(Sys.time())
  expires_at <- issued_at + 600  # valid for 10 minutes

  claims <- list(
    iss = client_email,
    scope = "https://www.googleapis.com/auth/cloud-platform",
    aud = token_uri,
    exp = expires_at,
    iat = issued_at
  )

  # Encode header and claims
  jwt_header <- base64url_encode(charToRaw(toJSON(header, auto_unbox = TRUE)))
  jwt_claims <- base64url_encode(charToRaw(toJSON(claims, auto_unbox = TRUE)))

  # JWT unsigned string
  jwt_unsigned <- paste(jwt_header, jwt_claims, sep = ".")

  # Sign the JWT using openssl
  key <- read_key(private_key)
  unsigned_raw <- charToRaw(jwt_unsigned)
  signature_raw <- signature_create(unsigned_raw, key = key, hash = sha256)
  signature <- base64url_encode(signature_raw)

  # Final signed JWT
  jwt <- paste(jwt_unsigned, signature, sep = ".")

  # Exchange JWT for access token
  res <- POST(
    url = token_uri,
    body = list(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion = jwt
    ),
    encode = "form"
  )

  # Parse and use access token
  token_data <- content(res)

  if (!is.null(token_data$access_token)) {
    faasr$ComputeServers[[server]]$AccessKey <- token_data$access_token
    succ_msg <- paste0("Successfully updated access key")
    cli_alert_success(succ_msg)
  } else {
    err_msg <- paste0("Error in creating access key using JWT.")
    cli_alert_danger(err_msg)
    stop()
  }
  return(faasr)
}


#' @title faasr_gcp_httr_request
#' @description 
#' function to send the curl request to the google cloud run api
#' by using the "httr" library. 
#' @param faasr a list form of the JSON file
#' @param server a string for the target server
#' @param action a string for the target action: /actions, /triggers, /rules
#' @param type REST API values; GET/PUT/DELETE/PATCH/POST
#' @param body a list of body
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param namespace a string for the specific namespace e.g., /whisk.system
#' @return response object from the google cloud run api
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

  # print(paste("Sending request to", endpoint))
  # send the REST request (POST/GET/PATCH/DELETE) with timeout
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


#' @title faasr_gcp_cloud_scheduler_httr_request
#' @description 
#' function to send the curl request to the google cloud scheduler
#' by using the "httr" library. 
#' @param faasr a list form of the JSON file
#' @param server a string for the target server
#' @param action a string for the target action: /actions, /triggers, /rules
#' @param type REST API values; GET/PUT/DELETE/PATCH/POST
#' @param body a list of body
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param namespace a string for the specific namespace e.g., /whisk.system
#' @return response object from the google cloud run api
#' @import httr
#' @import cli
#' @keywords internal

# help sending httr requests
faasr_gcp_cloud_scheduler_httr_request <- function(faasr, server, action, type, body=NULL, ssl=TRUE, namespace=NULL){

  if(is.null(faasr$ComputeServers[[server]]$SchedulerEndpoint)){
    endpoint <- "https://cloudscheduler.googleapis.com/v1/projects/"
  } else {
    endpoint <- faasr$ComputeServers[[server]]$SchedulerEndpoint
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

  # print(paste("Sending request to", endpoint))
  # send the REST request (POST/GET/PATCH/DELETE) with timeout
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
#' @param action a string for the target action
#' @param server a string for the target server
#' @param faasr a list form of the JSON file
#' @param all_jobs a list of all existing gcloud jobs
#' @return a logical value; if exists, return TRUE, 
#' doesn't exist, return FALSE
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_check_exists <- function(action, server, faasr, all_jobs){

  current_job_name <- paste0("projects/", faasr$ComputeServers[[server]]$Namespace, "/locations/", faasr$ComputeServers[[server]]$Region, "/jobs/", action)
  for(job in all_jobs)
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


#' @title faasr_register_workflow_google_cloud_check_user_input
#' @description 
#' Ask user input for overwriting the google cloud job
#' @param check a logical value for target existence
#' @param actionname a string for the target action name
#' @return a logical value for overwrite
#' @import cli
#' @keywords internal

faasr_register_workflow_google_cloud_check_user_input <- function(check, actionname){
  overwrite <- FALSE
  if (check){
    cli_alert_info(paste0("Do you want to overwrite the job?[y/n]"))

    while(TRUE) {
      check <- readline()
      if (check=="y" || check=="") {
        overwrite <- TRUE
        succ_msg <- paste0("Proceed to overwriting existing job - ", actionname)
        cli_alert_success(succ_msg)
        break
      } else if(check=="n") {
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
      }
    }
  }
  return(overwrite)
}


#' @title faasr_register_workflow_google_cloud_create_action
#' @description 
#' Create an action
#' if it already exists and user wants, update the action
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @param actionname a string for the target action name
#' @param server a string for the target server
#' @param faasr a list form of the JSON file
#' @param memory an integer for the max size of memory (MB)
#' @param timeout an integer for the max length of timeout (seconds)
#' @param check a logical value for target existence
#' @import httr
#' @import cli
#' @keywords internal

# create an action
faasr_register_workflow_google_cloud_create_action <- function(ssl, actionname, server, faasr, memory, timeout, check) {
  
  action <- paste0("/jobs?jobId=", actionname)
  request_type <- "POST"
  overwrite <- faasr_register_workflow_google_cloud_check_user_input(check, actionname)
  if (overwrite){
    # update the gcloud job, overwrites the parameters
    request_type <- "PATCH"
    action <- paste0("/jobs/", actionname)
  }

  # actioncontainer can be either default or user-customized
  if (length(faasr$ActionContainers[[actionname]])==0 || faasr$ActionContainers[[actionname]] == "") {
    actioncontainer <- "gcr.io/faasr-project/gcloud-job-tidyverse" # change this with the default faasr container
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
        timeout = paste0(timeout, "s"), # Set service timeout (e.g., "24 * 60 * 60 s" for 24 hours)
        serviceAccount = faasr$ComputeServers[[server]]$ClientEmail # linked service account
      )
    )
  )

  response <- faasr_gcp_httr_request(faasr, server, action, type=request_type, body=body, ssl)
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
  faasr <- faasr_refresh_gcp_accesskey(faasr, faas_name)

  # Prepare the args from the faasr object
  args <- jsonlite::toJSON(faasr, auto_unbox = TRUE)
  library(base64enc)
  encoded_args <- base64encode(charToRaw(args))

  # Build the body for the HTTP request without specifying the image
  body <- list(
    overrides = list(
      containerOverrides = list(
        list(
          args = encoded_args
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


#' @title faasr_set_workflow_timer_google_cloud
#' @description 
#' # set/unset workflow cron timer for google cloud
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param target a string for the target action
#' @param cron a string for cron data e.g., */5 * * * *
#' @param unset a logical value; set timer(FALSE) or unset timer(TRUE)
#' @param ssl SSL CA check; for the SSL certificate: FALSE
#' @import httr
#' @import cli
#' @keywords internal

# set workflow timer for google cloud
faasr_set_workflow_timer_gcp <- function(faasr, cred, target, cron, unset=FALSE, ssl=TRUE){

  # set variables
  server <- faasr$FunctionList[[target]]$FaaSServer
  scheduler_name <- paste0(target, "_trigger")
  action <- "/jobs"

  faasr <- faasr_replace_values(faasr, cred)
  faasr <- faasr_refresh_gcp_accesskey(faasr, server)

  response <- faasr_gcp_cloud_scheduler_httr_request(faasr, server, action, type="GET")
    if (response$status_code==200 || response$status_code==202){
        all_scheduler_jobs <- content(response)$jobs
        succ_msg <- paste0("All exising gcloud scheduler jobs fetched: ", length(content(response)$jobs), " jobs")
        cli_alert_success(succ_msg)
    } else {
        err_msg <- paste0("Error fetching all exising gcloud scheduler jobs: ", content(response)$error)
        cli_alert_danger(err_msg)
        stop()
    }
   
  # if unset==TRUE, delete the rule and trigger
  if (unset==FALSE){

    request_type <- "POST"
    check <- faasr_register_workflow_google_cloud_check_exists(scheduler_name, server, faasr, all_scheduler_jobs)
    
    overwrite <- faasr_register_workflow_google_cloud_check_user_input(check, scheduler_name)
    if (overwrite){
      # update the existing trigger
      request_type <- "PATCH"
      action <- paste0("/jobs/", scheduler_name)
    }

    namespace <- faasr$ComputeServers[[server]]$Namespace
    region <- faasr$ComputeServers[[server]]$Region
    job_endpoint <- faasr$ComputeServers[[server]]$Endpoint
    if (!startsWith(job_endpoint, "https://")){
      job_endpoint <- paste0("https://", job_endpoint)
    }
    job_endpoint <- paste0(job_endpoint, namespace, "/locations/", region, "/jobs/", target, ":run")

    faasr_args <- list(
      overrides = list(
        containerOverrides = list(
          list(
            args = base64encode(charToRaw(toJSON(faasr, auto_unbox = TRUE)))
          )
        )
        # taskCount = 1, # Configures as per need while invoking the job
        # timeout = paste0(service_timeout_seconds, "s")  # Set service timeout
      )
    )

    json_body <- toJSON(faasr_args, auto_unbox = TRUE)
    encoded_body <- base64encode(charToRaw(json_body))

    # Create a body
    body <- list(
      name = paste0("projects/", namespace, "/locations/", region, "/jobs/", scheduler_name),
      description = "FaaSr Cloud Scheduler",
      schedule = cron,
      timeZone = "UTC",
      httpTarget = list(
        httpMethod = "POST",
        uri = job_endpoint,
        body = encoded_body,  # Corrected encoding
        oauthToken = list(
          serviceAccountEmail = faasr$ComputeServers[[server]]$ClientEmail
        )
      )
    )
    response <- faasr_gcp_cloud_scheduler_httr_request(faasr, server, action, type=request_type, body=body, ssl)
    # response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully triggered gcloud scheduler job")
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error triggering gcloud scheduler job: ", content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }

  # if unset=FALSE, delete the cloud scheduler trigger
  } else {
    
    check <- faasr_register_workflow_google_cloud_check_exists(scheduler_name, server, faasr, all_scheduler_jobs)
    
    if (!check){
      err_msg <- paste0("Error: No gcloud scheduler job ", scheduler_name, " found")
      cli_alert_danger(err_msg)
      stop()
    }
    
    ## delete the gcloud scheduler job
    delete_action <- paste0("/jobs/", scheduler_name)
    response <- faasr_gcp_cloud_scheduler_httr_request(faasr, server, delete_action, type="DELETE")
    #response handling: status code
    if (response$status_code==200 || response$status_code==202){
      succ_msg <- paste0("Successfully deleted gcloud scheduler job - ", scheduler_name)
      cli_alert_success(succ_msg)
    } else {
      err_msg <- paste0("Error in deleting gcloud scheduler job - ", scheduler_name, ": ", content(response)$error)
      cli_alert_danger(err_msg)
      stop()
    }
  }
}
