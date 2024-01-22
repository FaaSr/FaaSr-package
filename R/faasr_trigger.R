#' @title Triggers the next User Functions of the Workflow, if any
#' @description Uses FaaS-specific APIs to generate triggers to execute downstream User Function
#' Currently supports:
#' * Apache OpenWhisk
#' * AWS Lambda
#' * GitHub Actions
#' @param faasr list with parsed and validated Payload

library("jsonlite")
library("httr")
library("paws")


faasr_trigger <- function(faasr) {

  # First extract the name of the user function
  user_function = faasr$FunctionInvoke

  # Find out which functions to InvokeNext
  invoke_next = faasr$FunctionList[[user_function]]$InvokeNext

  # Check if the list is empty or not
  if (length(invoke_next) == 0) {
    err_msg <- paste0('{\"faasr_trigger\":\"no triggers for ',user_function,'\"}', "\n")
    cat(err_msg)
    faasr_log(err_msg)
  } else {
    # Iterate through invoke_next and use FaaS-specific mechanisms to send trigger
    # use "for" loop to iteratively check functions in invoke_next list
    for (invoke_next_function in invoke_next) {

      # Change the FunctionInvoke to next function name
      faasr$FunctionInvoke <- invoke_next_function

      # Determine FaaS server name via faasr$FunctionList[[invoke_next_function]]$FaaSServer
      next_server <- faasr$FunctionList[[invoke_next_function]]$FaaSServer

      # Validate that FaaS server name exists in faasr$ComputeServers list
      if (next_server %in% names(faasr$ComputeServers)) {
        NULL
      } else {
      	err_msg <- paste0('{\"faasr_trigger\":\"invalid server name: ',next_server,'\"}', "\n")
        cat(err_msg)
        faasr_log(err_msg)
        break
      }

      # check FaaSType from the named compute server
      next_server_type <- faasr$ComputeServers[[next_server]]$FaaSType

      switch(next_server_type,
        # if OpenWhisk - use OpenWhisk API
        "OpenWhisk"={
          #
          # OpenWhisk API handling
          #
          # TBD - need to differentiate from IBMcloud or plain OpenWhisk
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
          url_2 <- paste0(endpoint, "/api/v1/namespace/",namespace,"/actions/",actionname,"?blocking=false&result=false")
          
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
            cat(succ_msg)
            faasr_log(succ_msg)
          } else {
            err_msg <- paste0('{\"faasr_trigger\":\"OpenWhisk: Error invoking: ', faasr$FunctionInvoke, ' - ', content(response)$error,'\"}\n')
            cat(err_msg)
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
          lambda <- paws::lambda(
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
          if (response$StatusCode == 200) {
            succ_msg <- paste0("faasr_trigger: Successfully invoked:", faasr$FunctionInvoke, "\n")
            cat(succ_msg)
            faasr_log(succ_msg)
          } else {
            err_msg <- paste0("faasr_trigger: Error invoking: ",faasr$FunctionInvoke," reason:", response$StatusCode, "\n")
            cat(err_msg)
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
            invoke_next_function <- paste0(invoke_next_function,".yml")
          }
	  workflow_file <- invoke_next_function
          git_ref <- faasr$ComputeServers[[next_server]]$Branch

	  # Set inputs for the workflow trigger event with InvocationID and Next_Invoke_Function_Name
          input_id <- faasr$InvocationID
          input_invokename <- faasr$FunctionInvoke
	  input_faasr_log <- faasr$FaaSrLog

          # The inputs for the workflow
          inputs <- list(
            ID = input_id,
            InvokeName = input_invokename,
	    FaaSrLog = input_faasr_log
          )

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
            cat(succ_msg)
            faasr_log(succ_msg)
          } else if (status_code(response) == 401) {
	    err_msg <- paste0("faasr_trigger: GitHub Action: Authentication failed, check the credentials\n")
            cat(err_msg)
            faasr_log(err_msg)
	  } else if (status_code(response) == 404) {
	    err_msg <- paste0("faasr_trigger: GitHub Action: Cannot find the destination, check the repo name: \"",repo,"\" and workflow name: \"",workflow_file,"\"\n")
            cat(err_msg)
            faasr_log(err_msg)
	  } else if (status_code(response) == 422) {
	    err_msg <- paste0("faasr_trigger: GitHub Action: Cannot find the destination, check the ref: ", faasr$FunctionInvoke, "\n")
            cat(err_msg)
            faasr_log(err_msg)
	  } else {
	    err_msg <- paste0("faasr_trigger: GitHub Action: unknown error happens when invoke next function\n")
            cat(err_msg)
            faasr_log(err_msg)
          }
    	}
      )
    }
  }
}


