#' @name faasr_trigger
#' @title faasr_trigger
#' @description 
#' Uses FaaS-specific APIs to generate triggers to execute downstream User Function
#' Currently supports:
#' * Apache OpenWhisk
#' * AWS Lambda
#' * GitHub Actions
#' * Google Cloud Platform
#' @param faasr list with parsed and validated Payload
#' @return return nothing / send requests to the FaaS servers.
#' @import jsonlite
#' @import httr
#' @importFrom "paws.compute" "lambda"
#' @importFrom "base64enc" "base64encode"
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' faasr_trigger(faasr)
#' }

faasr_trigger <- function(faasr) {

  # First extract the name of the user function
  user_function = faasr$FunctionInvoke

  # Find out which functions to InvokeNext
  invoke_next = faasr$FunctionList[[user_function]]$InvokeNext

  # Check if the list is empty or not
  if (length(invoke_next) == 0) {
    err_msg <- paste0('{\"faasr_trigger\":\"no triggers for ',user_function,'\"}', "\n")
    message(err_msg)
    faasr_log(err_msg)
  } else {
    
    function_result <- faasr$FunctionResult
    
    # Iterate through invoke_next and use FaaS-specific mechanisms to send trigger
    # use "for" loop to iteratively check functions in invoke_next list
    for (invoke_next_string in invoke_next) {
      
      # Parse the string to extract function, condition, and rank
      parsed <- faasr_parse_invoke_next_string(invoke_next_string)
      invoke_next_function <- parsed$func_name
      condition <- parsed$condition
      rank_num <- parsed$rank
      
      # Evaluate condition before proceeding
      if (!faasr_evaluate_condition(condition, function_result)) {
        skip_msg <- paste0('{\"faasr_trigger\":\"skipping ',invoke_next_function,' - condition [',condition,'] not met (result: ',function_result,')\"}', "\n")
        message(skip_msg)
        faasr_log(skip_msg)
        next  # Skip this trigger
      }
      
      # Log successful condition match
      if (!is.null(condition)) {
        cond_msg <- paste0('{\"faasr_trigger\":\"condition [',condition,'] met for ',invoke_next_function,' - proceeding with trigger\"}', "\n")
        message(cond_msg)
        faasr_log(cond_msg)
      }
      
      # Change the FunctionInvoke to next function name
      faasr$FunctionInvoke <- invoke_next_function

      # Determine FaaS server name via faasr$FunctionList[[invoke_next_function]]$FaaSServer
      next_server <- faasr$FunctionList[[invoke_next_function]]$FaaSServer

      for (rank in 1:rank_num){
        if (rank_num > 1){
          faasr$FunctionList[[invoke_next_function]]$Rank <- paste0(rank,"/",rank_num)
        }

        # Validate that FaaS server name exists in faasr$ComputeServers list
        if (next_server %in% names(faasr$ComputeServers)) {
          NULL
        } else {
      	  err_msg <- paste0('{\"faasr_trigger\":\"invalid server name: ',next_server,'\"}', "\n")
          message(err_msg)
          faasr_log(err_msg)
          break
        }

        # check FaaSType from the named compute server
        next_server_type <- faasr$ComputeServers[[next_server]]$FaaSType

        switch(next_server_type,
          "GoogleCloud"={
            #
            # GoogleCloud API handling
            #
            # Set the env values for the google cloud action.
            # GoogleCloud API handling
            endpoint <- faasr$ComputeServers[[next_server]]$Endpoint
            namespace <- faasr$ComputeServers[[next_server]]$Namespace
            region <- faasr$ComputeServers[[next_server]]$Region
            actionname <- invoke_next_function
            endpoint <- paste0(endpoint, namespace, "/locations/", region, "/jobs/", actionname, ":run")

            faasr <- faasr_refresh_gcp_accesskey(faasr, next_server)
            token <- faasr$ComputeServers[[next_server]]$AccessKey

            if (is.null(faasr$ComputeServers[[next_server]]$SSL) || faasr$ComputeServers[[next_server]]$SSL == "") {
              ssl <- TRUE
            } else {
              ssl <- as.logical(toupper(faasr$ComputeServers[[next_server]]$SSL))
            }

            headers <- c(
              'accept' = 'application/json',
              'Content-Type' = 'application/json',
              'Authorization' = paste("Bearer", token)  # Set Authorization header directly
            )

            # Prepare the args from the faasr object
            args <- toJSON(faasr, auto_unbox = TRUE)
            
            #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            # Base64 encode the JSON payload before sending
            encoded_args <- base64encode(charToRaw(args))
            #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            # Build the body for the HTTP request, using the encoded payload
            body <- list(
              overrides = list(
                containerOverrides = list(
                  list(
                    # Use the new encoded string here
                    args = encoded_args
                  )
                )
                # taskCount = 1, (Can be set if needed)
                # timeout = paste0(service_timeout_seconds, "s")  # Set service timeout
              )
            )

            # Send the REST request (POST/GET/PUT/PATCH)
            response <- POST(
              url = endpoint,
              add_headers(.headers = headers),
              body = body,
              encode = "json",
              httr::config(ssl_verifypeer = ssl, ssl_verifyhost = ssl),
              accept_json()
            )

            if (response$status_code == 200 || response$status_code == 202) {
              succ_msg <- paste0('{\"faasr_trigger\":\"GoogleCloud: Successfully invoked: ', faasr$FunctionInvoke, '\"}\n')
              message(succ_msg)
              faasr_log(succ_msg)
            } else {
              err_msg <- paste0('{\"faasr_trigger\":\"GoogleCloud: Error invoking: ', faasr$FunctionInvoke, '\"}\n')
              message(response)
              faasr_log(err_msg)
            }
          },
          # if OpenWhisk - use OpenWhisk API
          "OpenWhisk"={
            #
            # OpenWhisk API handling
            #
            # Set the env values for the openwhisk action.
            endpoint <- faasr$ComputeServers[[next_server]]$Endpoint
            api_key <- faasr$ComputeServers[[next_server]]$API.key
            api_key <- strsplit(api_key, ":")[[1]]
            if (is.null(faasr$ComputeServers[[next_server]]$SSL) || faasr$ComputeServers[[next_server]]$SSL ==""){
              ssl <- TRUE
            } else{
              ssl <- as.logical(toupper(faasr$ComputeServers[[next_server]]$SSL))
            }
            namespace <- faasr$ComputeServers[[next_server]]$Namespace
            actionname <- invoke_next_function

            if (!startsWith(endpoint, "https") && !startsWith(endpoint, "http")){
              endpoint <- paste0("https://", endpoint)
            }
            url_2 <- paste0(endpoint, "/api/v1/namespaces/",namespace,"/actions/",actionname,"?blocking=false&result=false")
            
            headers <- c(
              'accept' = 'application/json', 
              'Content-Type' = 'application/json'
            )

            # send the REST request(POST/GET/PUT/PATCH)
            response <- POST(
              url = url_2,
              authenticate(api_key[1], api_key[2]),
              add_headers(.headers = headers),
              body=faasr,
              encode="json",
              httr::config(ssl_verifypeer = ssl, ssl_verifyhost = ssl),
              accept_json()
            )

            if (response$status_code==200 || response$status_code==202){
              succ_msg <- paste0('{\"faasr_trigger\":\"OpenWhisk: Successfully invoked: ', faasr$FunctionInvoke, '\"}\n')
              message(succ_msg)
              faasr_log(succ_msg)
            } else {
              err_msg <- paste0('{\"faasr_trigger\":\"OpenWhisk: Error invoking: ', faasr$FunctionInvoke, ' - ', content(response)$error,'\"}\n')
              message(err_msg)
              faasr_log(err_msg)
            }
          },
          
          # if AWS Lambda - use Lambda API
          "Lambda"={
            # AWS Lambda API handling
            # get next function server
            target_server <- faasr$ComputeServers[[next_server]]

            # prepare env variables for lambda
            
            # set invoke request body, it should be a JSON. To pass the payload, toJSON is required.
            payload_json <- toJSON(faasr, auto_unbox = TRUE)

            # Create a Lambda client using paws
            lambda <- paws.compute::lambda(
              config=list(
                credentials=list(
                  creds=list(
                    access_key_id=target_server$AccessKey,
                    secret_access_key=target_server$SecretKey,
                    session_token=""
                  )
                ),
                region=target_server$Region
              )
            )

      # Invoke next function with FunctionName and Payload, receive trigger response
            next_lambda_function_name <- invoke_next_function

      # Invoke next function with FunctionName and Payload, receive trigger response
            response <- lambda$invoke_async(
              FunctionName = next_lambda_function_name,
              InvokeArgs = payload_json
            )

      # Check if next function be invoked successfully
            if (response$Status == 202) {
              succ_msg <- paste0("faasr_trigger: Successfully invoked:", faasr$FunctionInvoke, "\n")
              message(succ_msg)
              faasr_log(succ_msg)
            } else {
              err_msg <- paste0("faasr_trigger: Error invoking: ",faasr$FunctionInvoke," reason:", response$Status, "\n")
              message(err_msg)
              faasr_log(err_msg)
            }
          },

        # if GitHub Actions - use GH Actions
          "GitHubActions"={
            # GitHub Actions API handling
            # Set env values for GitHub Actions event
            pat <- faasr$ComputeServers[[next_server]]$Token
            username <- faasr$ComputeServers[[next_server]]$UserName
            reponame <- faasr$ComputeServers[[next_server]]$ActionRepoName
            repo <- paste0(username, "/", reponame)
            if (!endsWith(invoke_next_function,".yml") && !endsWith(invoke_next_function,".yaml")){
              workflow_file <- paste0(invoke_next_function,".yml")
            } else {
              workflow_file <- invoke_next_function
            }
            git_ref <- faasr$ComputeServers[[next_server]]$Branch

            # Make a copy of faasr
            faasr_git <- faasr
            
            # Hide all credentials before sending the payload to the github actions
            for (faas_js in names(faasr_git$ComputeServers)){
              switch (faasr_git$ComputeServers[[faas_js]]$FaaSType,
                "GitHubActions"={
                  faasr_git$ComputeServers[[faas_js]]$Token <- paste0(faas_js,"_TOKEN")
                },
                "Lambda"={
                  faasr_git$ComputeServers[[faas_js]]$AccessKey <- paste0(faas_js,"_ACCESS_KEY")
                  faasr_git$ComputeServers[[faas_js]]$SecretKey <- paste0(faas_js,"_SECRET_KEY")
                },
                "OpenWhisk"={
                  faasr_git$ComputeServers[[faas_js]]$API.key <- paste0(faas_js,"_API_KEY")
                }
              )
              }
            for (data_js in names(faasr_git$DataStores)){
              faasr_git$DataStores[[data_js]]$AccessKey <- paste0(data_js,"_ACCESS_KEY")
              faasr_git$DataStores[[data_js]]$SecretKey <- paste0(data_js,"_SECRET_KEY")
            }
            # The inputs for the workflow
            inputs <- list(
            PAYLOAD = jsonlite::toJSON(faasr_git, auto_unbox=TRUE)
            )

            # Delete the copy to ensure the memory
            remove(faasr_git)

            # Set the URL for the REST API endpoint of next action
            url <- paste0("https://api.github.com/repos/", repo, "/actions/workflows/", workflow_file, "/dispatches")

            # Set the body of the POST request with github ref and inputs
            body <- list(
              ref = git_ref,
              inputs = inputs
            )

            # Use httr::POST to send the POST request
      # Reference link for POST request: https://docs.github.com/en/rest/actions/workflows?apiVersion=2022-11-28
            response <- POST(
              url = url,
              body = body,
              encode = "json",
              add_headers(
                Authorization = paste("token", pat),
                Accept = "application/vnd.github.v3+json",
                "X-GitHub-Api-Version" = "2022-11-28"
              )
            )

      # Check if next action be invoked successfully
            if (status_code(response) == 204) {
              succ_msg <- paste0("faasr_trigger: GitHub Action: Successfully invoked:", faasr$FunctionInvoke, "\n")
              message(succ_msg)
              faasr_log(succ_msg)
            } else if (status_code(response) == 401) {
        err_msg <- paste0("faasr_trigger: GitHub Action: Authentication failed, check the credentials\n")
              message(err_msg)
              faasr_log(err_msg)
      } else if (status_code(response) == 404) {
        err_msg <- paste0("faasr_trigger: GitHub Action: Cannot find the destination, check the repo name: \"",repo,"\" and workflow name: \"",workflow_file,"\"\n")
              message(err_msg)
              faasr_log(err_msg)
      } else if (status_code(response) == 422) {
        err_msg <- paste0("faasr_trigger: GitHub Action: Cannot find the destination, check the ref: ", faasr$FunctionInvoke, "\n")
              message(err_msg)
              faasr_log(err_msg)
      } else {
        err_msg <- paste0("faasr_trigger: GitHub Action: unknown error happens when invoke next function\n")
              message(err_msg)
              faasr_log(err_msg)
            }
        },
      
      "SLURM"={
        # SLURM REST API handling
        server_info <- faasr$ComputeServers[[next_server]]
        api_version <- server_info$APIVersion %||% "v0.0.37"
        endpoint <- server_info$Endpoint
        
        if (!startsWith(endpoint, "http")) {
          endpoint <- paste0("http://", endpoint)
        }
        
        token_validation <- faasr_validate_jwt_token(server_info$Token)
        if (!token_validation$valid) {
          err_msg <- paste0('{\"faasr_trigger\":\"SLURM: Token validation failed for ', 
                            next_server, ' - ', token_validation$error, '\"}', "\n")
          message(err_msg)
          faasr_log(err_msg)
          
          # Mark workflow as failed due to authentication
          faasr$FunctionResult <- "FAILED_AUTH"
          stop(paste0("SLURM authentication failed: ", token_validation$error))
        }
        
        username <- server_info$UserName %||% "ubuntu"
        if (is.null(username) || username == "") {
          err_msg <- paste0('{\"faasr_trigger\":\"SLURM: Username not configured for server ', 
                            next_server, '\"}', "\n")
          message(err_msg)
          faasr_log(err_msg)
          stop("SLURM username not configured")
        }
        
        # Create job script
        job_script <- faasr_slurm_create_job_script(faasr, invoke_next_function)
        
        # Prepare job payload
        job_payload <- list(
          "job" = list(  # Add quotes around "job"
            "name" = paste0("faasr-", invoke_next_function),
            "partition" = server_info$Partition %||% "faasr",
            "nodes" = as.character(as.integer(server_info$Nodes %||% 1)),           # Convert to string
            "tasks" = as.character(as.integer(server_info$Tasks %||% 1)),           # Convert to string
            "cpus_per_task" = as.character(as.integer(server_info$CPUsPerTask %||% 1)), # Convert to string
            "memory_per_cpu" = as.character(as.integer(server_info$Memory %||% 1024)),  # Convert to string
            "time_limit" = as.character(as.integer(server_info$TimeLimit %||% 60)),     # Convert to string
            "current_working_directory" = server_info$WorkingDirectory %||% "/tmp",
            "environment" = list(
              "FAASR_PAYLOAD" = jsonlite::toJSON(faasr, auto_unbox = TRUE, pretty = FALSE)  # Add pretty=FALSE
            )
          ),
          "script" = job_script  # Move s to end and add quotescript
        )
        
        # Submit job
        submit_url <- paste0(endpoint, "/slurm/", api_version, "/job/submit")
        
        headers <- c(
          'Accept' = 'application/json',
          'Content-Type' = 'application/json'
        )
        
        if (!is.null(server_info$Token) && server_info$Token != "") {
          headers['X-SLURM-USER-TOKEN'] <- server_info$Token
          # Add username header - use configured username or default to 'ubuntu'
          username <- server_info$UserName %||% "ubuntu"
          headers['X-SLURM-USER-NAME'] <- username
        }
        else {
          err_msg <- paste0('{\"faasr_trigger\":\"SLURM: No authentication token available for server ', next_server, '\"}', "\n")
          message(err_msg)
          faasr_log(err_msg)
          next  # Skip this trigger instead of failing
        }
        
        response <- faasr_slurm_httr_request(
          endpoint = submit_url,
          method = "POST", 
          headers = headers,
          body = job_payload
        )
        
        if (response$status_code %in% c(200, 201, 202)) {
          job_info <- httr::content(response, "parsed")
          job_id <- job_info$job_id %||% 
            job_info$jobId %||% 
            job_info$id %||% 
            (if(!is.null(job_info$job)) job_info$job$job_id else NULL) %||%
            "unknown"
          
          succ_msg <- paste0('{\"faasr_trigger\":\"SLURM: Successfully submitted job: ', 
                             faasr$FunctionInvoke, ' (Job ID: ', job_id, ')\"}', "\n")
          message(succ_msg)
          faasr_log(succ_msg)
        } else {
          error_content <- httr::content(response, "text")
          err_msg <- paste0('{\"faasr_trigger\":\"SLURM: Error submitting job: ', 
                            faasr$FunctionInvoke, ' - HTTP ', response$status_code, 
                            ': ', error_content, '\"}', "\n")
          message(err_msg)
          faasr_log(err_msg)
          
          if (response$status_code == 401) {
            debug_msg <- paste0('{\"faasr_trigger\":\"SLURM: Authentication failed - check token validity and username\"}', "\n")
            message(debug_msg)
            faasr_log(debug_msg)
          }
        }
      }
      
        )
      }
    }
  }
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
    message(succ_msg)
    #faasr_log(succ_msg)
  } else {
    err_msg <- paste0("Error in creating access key using JWT.")
    message(err_msg)
    #faasr_log(err_msg)
    stop()
  }
  return(faasr)
}
