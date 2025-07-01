# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @name faasr_register_workflow_slurm
#' @title faasr_register_workflow_slurm
#' @description 
#' Register workflow for SLURM cluster.
#' Parse faasr to get the server list and actions.
#' Verify connectivity to SLURM REST API endpoints.
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @import httr
#' @import cli
#' @keywords internal
#' 

faasr_register_workflow_slurm <- function(faasr, cred) {
  
  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")
  
  # Get SLURM action list
  action_list <- faasr_register_workflow_slurm_action_lists(faasr)
  
  if (length(action_list) == 0) {
    return("")
  }
  
  # Check servers and test connectivity
  for (server in names(action_list)) {
    
    cli::cli_h1(paste0("Registering workflow for SLURM: ", server))
    cli::cli_progress_bar(
      format = paste0(
        "FaaSr {pb_spin} Registering workflow SLURM ",
        "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
      ),
      format_done = paste0(
        "{col_yellow(symbol$checkbox_on)} Successfully registered actions for server {server} ",
        "in {pb_elapsed}."
      ),
      total = length(action_list[[server]]) + 1  # +1 for connectivity test
    )
    
    # Replace dummy credentials with real ones for testing
    #faasr_test <- faasr_replace_values(faasr, cred)
    
    # Test SLURM connectivity
    check <- faasr_register_workflow_slurm_check_connectivity(server, faasr)
    if (!check) {
      cli_alert_danger(paste0("Failed to connect to SLURM server: ", server))
      stop()
    }
    cli_progress_update()
    
    # Validate each action (mostly connectivity validation)
    for (action in action_list[[server]]) {
      cli_alert_success(paste0("Validated action: ", action))
      cli_progress_update()
    }
  }
  
  cli_text(col_cyan("{symbol$menu} {.strong Successfully registered all SLURM actions}"))
}

#' @title faasr_register_workflow_slurm_action_lists
#' @description 
#' Parse the faasr and get the list of function:server
#' Find actions which are using "SLURM"
#' @param faasr a list form of the JSON file
#' @return a list of "server name: action names" pairs
#' @keywords internal

faasr_register_workflow_slurm_action_lists <- function(faasr) {
  # empty list
  action_list <- list()
  
  # for each function, iteratively collect server names and action names
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    
    # Check if server exists
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)) {
      err_msg <- paste0("Invalid server:", server_name, " check server type")
      cli_alert_danger(err_msg)
      stop()
    }
    
    # if FaaStype is SLURM, add it to the list
    if (faasr$ComputeServers[[server_name]]$FaaSType == "SLURM") {
      action_name <- fn
      action_list[[server_name]] <- unique(c(action_list[[server_name]], action_name))
    }
  }
  
  return(action_list)
}

#' @title faasr_register_workflow_slurm_check_connectivity
#' @description 
#' Check the SLURM REST API connectivity by sending a ping request
#' @param server a string for the target server name
#' @param faasr a list form of the JSON file with credentials replaced
#' @return a logical value; if successful, return TRUE, otherwise FALSE
#' @keywords internal

faasr_register_workflow_slurm_check_connectivity <- function(server, faasr) {
  
  server_info <- faasr$ComputeServers[[server]]
  endpoint <- server_info$Endpoint
  api_version <- server_info$APIVersion %||% "v0.0.37"  # Default to v0.0.37
  
  # Ensure endpoint has protocol
  if (!startsWith(endpoint, "http")) {
    endpoint <- paste0("http://", endpoint)
  }
  
  # Test ping endpoint
  ping_url <- paste0(endpoint, "/slurm/", api_version, "/ping")
  
  # Prepare headers
  headers <- c('Accept' = 'application/json')
  
  # Add JWT token and username if available
  if (!is.null(server_info$Token) && server_info$Token != "") {
    headers['X-SLURM-USER-TOKEN'] <- server_info$Token
    # Add username header - use configured username or default to 'ubuntu'
    username <- server_info$UserName %||% "ubuntu"
    headers['X-SLURM-USER-NAME'] <- username
  }
  
  tryCatch({
    response <- faasr_slurm_httr_request(
      endpoint = ping_url,
      method = "GET",
      headers = headers
    )
    
    if (response$status_code == 200) {
      cli_alert_success(paste0("SLURM connectivity test passed for: ", server))
      return(TRUE)
    } else {
      err_msg <- paste0("SLURM connectivity test failed: HTTP ", response$status_code)
      response_content <- content(response, "text")
      err_msg <- paste0(err_msg, " - ", response_content)
      cli_alert_danger(err_msg)
      return(FALSE)
    }
    
  }, error = function(e) {
    err_msg <- paste0("SLURM connectivity error: ", as.character(e))
    cli_alert_danger(err_msg)
    return(FALSE)
  })
}

#' @title faasr_workflow_invoke_slurm
#' @description 
#' Invoke a workflow for SLURM
#' This function is invoked by faasr_workflow_invoke
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param faas_name a string for the target server
#' @param actionname a string for the target action name
#' @import httr
#' @import cli
#' @keywords internal

faasr_workflow_invoke_slurm <- function(faasr, cred, faas_name, actionname) {
  
  # Replace credentials
  faasr_with_creds <- faasr_replace_values(faasr, cred)
  server_info <- faasr_with_creds$ComputeServers[[faas_name]]
  
  token_validation <- faasr_validate_jwt_token(server_info$Token)
  if (!token_validation$valid) {
    err_msg <- paste0("SLURM token validation failed: ", token_validation$error)
    cli::cli_alert_danger(err_msg)
    stop(err_msg)
  }
  
  # Validate username configuration
  username <- server_info$UserName %||% "ubuntu"
  if (is.null(username) || username == "") {
    err_msg <- paste0('{\"SLURM: Username not configured for server ', 
                      faas_name, '\"}', "\n")
    #err_msg <- paste0("SLURM token validation failed: ", token_validation$error)
    cli::cli_alert_danger(err_msg)
    stop(err_msg)
  }
  
  tryCatch({
    parts <- strsplit(server_info$Token, "\\.")[[1]]
    payload <- parts[2]
    payload <- paste0(payload, paste0(rep("=", 4 - (nchar(payload) %% 4)), collapse = ""))
    decoded <- jsonlite::fromJSON(rawToChar(base64enc::base64decode(payload)))
    exp_time <- as.POSIXct(decoded$exp, origin = "1970-01-01", tz = "UTC")
    cli::cli_alert_info(paste0("Token expires at: ", format(exp_time)))
  }, error = function(e) {
    # Ignore token parsing errors for logging
  })
  
  endpoint <- server_info$Endpoint
  api_version <- server_info$APIVersion %||% "v0.0.40"
  if (!startsWith(endpoint, "http")) {
    endpoint <- paste0("http://", endpoint)
  }
  
  # Create job script for FaaSr execution
  job_script <- faasr_slurm_create_job_script(faasr_with_creds, actionname)
  
  # Get job parameters from server configuration with defaults
  partition <- server_info$Partition %||% "faasr"
  nodes <- as.integer(server_info$Nodes %||% 1)
  tasks <- as.integer(server_info$Tasks %||% 1)
  cpus_per_task <- as.integer(server_info$CPUsPerTask %||% 1)
  memory_mb <- as.integer(server_info$Memory %||% 1024)
  time_limit <- as.integer(server_info$TimeLimit %||% 60)
  working_dir <- server_info$WorkingDirectory %||% "/tmp"
  
  job_payload <- list(
    "job" = list(  # Add quotes around "job" like trigger
      "name" = paste0("faasr-", actionname),
      "partition" = server_info$Partition %||% "faasr",
      "nodes" = as.character(as.integer(server_info$Nodes %||% 1)),           # Convert to string like trigger
      "tasks" = as.character(as.integer(server_info$Tasks %||% 1)),           # Convert to string like trigger
      "cpus_per_task" = as.character(as.integer(server_info$CPUsPerTask %||% 1)), # Convert to string like trigger
      "memory_per_cpu" = as.character(as.integer(server_info$Memory %||% 1024)),  # Convert to string like trigger
      "time_limit" = as.character(as.integer(server_info$TimeLimit %||% 60)),     # Convert to string like trigger
      "current_working_directory" = server_info$WorkingDirectory %||% "/tmp",
      "environment" = list(
        "FAASR_PAYLOAD" = jsonlite::toJSON(faasr_with_creds, auto_unbox = TRUE, pretty = FALSE)  # Add pretty=FALSE like trigger
      )
    ),
    "script" = job_script  # Move script to end like trigger
  )
  
  # Submit job via REST API
  submit_url <- paste0(endpoint, "/slurm/", api_version, "/job/submit")
  
  headers <- c(
    'Accept' = 'application/json',
    'Content-Type' = 'application/json'
  )
  
  # Add JWT token and username - BOTH are required
  if (!is.null(server_info$Token) && server_info$Token != "") {
    headers['X-SLURM-USER-TOKEN'] <- server_info$Token
    username <- server_info$UserName %||% "ubuntu"
    headers['X-SLURM-USER-NAME'] <- username
  } else {
    cli_alert_danger("No SLURM token provided - authentication will fail")
  }
  
  # Debug logging
  cat("Submitting job to:", submit_url, "\n")
  cat("Token present:", !is.null(server_info$Token) && server_info$Token != "", "\n")
  
  response <- faasr_slurm_httr_request(
    endpoint = submit_url,
    method = "POST",
    headers = headers,
    body = job_payload
  )
  
  # Enhanced response handling
  if (response$status_code %in% c(200, 201, 202)) {
    job_info <- content(response)
    
    
    if (!is.null(job_info$errors) && length(job_info$errors) > 0) {
      # Extract error details
      error_details <- job_info$errors[[1]]
      error_code <- error_details$error_code
      error_message <- error_details$error
      
      # Handle specific SLURM error codes
      if (error_code == 5005) {
        err_msg <- paste0("SLURM Authentication Error: Invalid or expired token - ", error_message)
        cli_alert_danger(err_msg)
        stop(err_msg)
      } else if (error_code %in% c(5001, 5002, 5003, 5004)) {
        err_msg <- paste0("SLURM Authentication/Authorization Error (Code: ", error_code, "): ", error_message)
        cli_alert_danger(err_msg)
        stop(err_msg)
      } else {
        err_msg <- paste0("SLURM Error (Code: ", error_code, "): ", error_message)
        cli_alert_danger(err_msg)
        stop(err_msg)
      }
    }
    
    # Better job ID extraction with debugging
    job_id <- NULL
    if (!is.null(job_info$job_id)) {
      job_id <- job_info$job_id
    } else if (!is.null(job_info$jobId)) {  # Alternative field name
      job_id <- job_info$jobId
    } else if (!is.null(job_info$id)) {  # Another alternative
      job_id <- job_info$id
    }
    
    if (is.null(job_id)) {
      # Debug: Print entire response to understand structure
      cat("Job ID not found in response. Full response:\n")
      cat(jsonlite::toJSON(job_info, pretty = TRUE), "\n")
      job_id <- "unknown"
    }
    
    succ_msg <- paste0("Successfully submitted SLURM job: ", actionname, ", Job ID: ", job_id)
    cli_alert_success(succ_msg)
  } else {
    error_content <- content(response, "text")
    err_msg <- paste0("Error submitting SLURM job: ", actionname, " - HTTP ", response$status_code, ": ", error_content)
    cli_alert_danger(err_msg)
    
    # Additional debugging for authentication errors
    if (response$status_code == 401) {
      cli_alert_danger("HTTP 401: Authentication failed - check token validity and expiration")
    } else if (response$status_code == 403) {
      cli_alert_danger("HTTP 403: Authorization failed - check user permissions")
    } else if (response$status_code == 400) {
      cli_alert_danger("HTTP 400: Bad Request - check job payload format")
    }
    
    stop(err_msg)
  }
}

#' @title faasr_slurm_create_job_script
#' @description 
#' Create SLURM job script for FaaSr execution
#' @param faasr a list form of the JSON file
#' @param actionname a string for the action name
#' @return string containing the job script
#' @keywords internal

faasr_slurm_create_job_script <- function(faasr, actionname) {
  
  # Get container image with fallback to default
  container_image <- "faasr/openwhisk-tidyverse:latest"
  if (!is.null(faasr$ActionContainers[[actionname]]) && 
      faasr$ActionContainers[[actionname]] != "") {
    container_image <- faasr$ActionContainers[[actionname]]
  }
  
  # Create job script following FaaSr patterns
  script_lines <- c(
    "#!/bin/bash",
    paste0("#SBATCH --job-name=faasr-", actionname),
    paste0("#SBATCH --output=faasr-", actionname, "-%j.out"),
    paste0("#SBATCH --error=faasr-", actionname, "-%j.err"),
    "",
    "# FaaSr SLURM Job Script",
    paste0("echo \"Starting FaaSr job: ", actionname, "\""),
    "echo \"Job ID: $SLURM_JOB_ID\"",
    "echo \"Node: $SLURMD_NODENAME\"", 
    "echo \"Time: $(date)\"",
    "",
    "# Check container runtime availability",
    "if command -v docker &> /dev/null; then",
    "    CONTAINER_CMD=\"docker\"",
    "elif command -v podman &> /dev/null; then",
    "    CONTAINER_CMD=\"podman\"",
    "else",
    "    echo \"Error: Neither Docker nor Podman found\"",
    "    exit 1",
    "fi",
    "",
    "echo \"Using container runtime: $CONTAINER_CMD\"",
    "",
    "# Execute FaaSr container with payload",
    "$CONTAINER_CMD run --rm --network=host \\",
    "  -e FAASR_PAYLOAD=\"$FAASR_PAYLOAD\" \\",
    paste0("  ", container_image, " \\"),
    "  /bin/bash -c 'cd /action && Rscript faasr_start_invoke_slurm.R'",
    "",
    paste0("echo \"FaaSr job completed: ", actionname, "\""),
    "echo \"End time: $(date)\""
  )
  
  return(paste(script_lines, collapse = "\n"))
}

#' @title faasr_slurm_httr_request
#' @description 
#' Helper function to send HTTP requests to SLURM REST API
#' @param endpoint full URL endpoint
#' @param method HTTP method
#' @param headers HTTP headers
#' @param body request body (optional)
#' @return HTTP response object
#' @keywords internal

faasr_slurm_httr_request <- function(endpoint, method = "GET", headers = NULL, body = NULL) {
  
  # Select HTTP function
  http_func <- switch(toupper(method),
                      "GET" = GET,
                      "POST" = POST, 
                      "PUT" = PUT,
                      "DELETE" = DELETE,
                      stop("Unsupported HTTP method: ", method)
  )
  
  # Make request
  if (is.null(body)) {
    response <- http_func(
      url = endpoint,
      add_headers(.headers = headers),
      timeout(30)
    )
  } else {
    response <- http_func(
      url = endpoint,
      add_headers(.headers = headers),
      body = body,
      encode = "json", 
      timeout(30)
    )
  }
  
  return(response)
}