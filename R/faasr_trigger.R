#' @title Triggers the next User Functions of the Workflow, if any
#' @description Uses FaaS-specific APIs to generate triggers to execute downstream User Function
#' Currently supports:
#' * Apache OpenWhisk
#' * AWS Lambda
#' * GitHub Actions
#' @param faasr list with parsed and validated Payload

library("jsonlite")
library("RCurl")
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
          api_key <- faasr$ComputeServers[[next_server]]$API.key
	  region <- faasr$ComputeServers[[next_server]]$Region
          namespace <- faasr$ComputeServers[[next_server]]$Namespace
          actionname <- invoke_next_function

	  # Openwhisk with IBM cloud - Get a token by using the API key
	  # URL is the ibmcloud's iam center.
	  url <- "https://iam.cloud.ibm.com/identity/token"

	  # Body contains authorization type and api key
	  body <- list(grant_type = "urn:ibm:params:oauth:grant-type:apikey",apikey=api_key)

	  # Header is HTTR request's header.
	  headers <- c("Content-Type"="application/x-www-form-urlencoded")

          # Use httr::POST to send the POST request to the IBMcloud iam centers to get a token.
	  response <- POST(url = url,body = body,encode = "form",add_headers(.headers = headers))

	  # Parse the result to get a token.
	  result <- content(response, as = "parsed")

	  # if result returns no error(length is 0), define token.
	  if (length(result$errorMessage) == 0) {
	    token <- paste("Bearer",result$access_token)
	    # if result returns an error, return an error message and stop.
	  } else {
	    err_msg <- paste0('{\"faasr_trigger\":\"unable to invoke next action, authentication error\"}', "\n")
	    cat(err_msg)
	    faasr_log(err_msg)
	    break
	  }

	  # Openwhisk - Invoke next action - action name should be described.
	  # Reference: https://cloud.ibm.com/apidocs/functions
	  # URL is a form of "https://region.functions.cloud.ibm.cloud/api/v1/namespaces/namespace/actions/actionname",
	  # blocking=TRUE&result=TRUE is optional
	  url_2 <- paste0("https://",region,".functions.cloud.ibm.com/api/v1/namespaces/",namespace,"/actions/",actionname,"?blocking=false&result=false")

	  # header is HTTR request headers
	  headers_2 <- c("accept"="application/json", "authorization"=token, "content-type"="application/json")

	  # data is a body and it should be a JSON. To pass the payload, toJSON is required.
	  data_2 <- toJSON(faasr, auto_unbox=TRUE)

	  # Make one option for invoking RCurl
	  curl_opts_2 <- list(post=TRUE, httpheader=headers_2, postfields=data_2)

	  # Perform RCurl::curlPerform to send the POST request to IBMcloud function server.
	  response_2 <- curlPerform(url=url_2, .opts=curl_opts_2)
	},
				
       	# if AWS Lambda - use Lambda API
	"Lambda"={
          # AWS Lambda API handling
	  # get next function server
          target_server <- faasr$ComputeServers[[next_server]]

	  # prepare env variables for lambda
          Sys.setenv("AWS_ACCESS_KEY_ID"=target_server$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_server$SecretKey, "AWS_DEFAULT_REGION"=target_server$Region, "AWS_SESSION_TOKEN" = "")

	  # set invoke request body, it should be a JSON. To pass the payload, toJSON is required.
	  payload_json <- toJSON(faasr, auto_unbox = TRUE)

	  # Create a Lambda client using paws
          lambda <- paws::lambda()

	  # Invoke next function with FunctionName and Payload, receive trigger response
          next_lambda_function_name <- invoke_next_function

	  # Invoke next function with FunctionName and Payload, receive trigger response
          response <- lambda$invoke(
            FunctionName = next_lambda_function_name,
            Payload = payload_json
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
          git_ref <- faasr$ComputeServers[[next_server]]$Ref

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


