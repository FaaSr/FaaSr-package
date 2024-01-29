#' @title Client tool for registering aws-lambda functions.
#' @description faasr_register_workflow_aws_lambda: 
#' starts to register functions in the JSON file for AWS-Lambda
#' @description faasr_register_workflow_lambda_function_lists: 
#' Get aws lambda function list              
#' @description faasr_register_workflow_lambda_function_image: 
#' Get aws lambda function image list              
#' @description faasr_register_workflow_aws_ecr_build: 
#' Build aws ecr repository with the given json              
#' @description faasr_register_workflow_aws_lambda_role_create: 
#' create lambda role             
#' @description faasr_register_workflow_aws_ecr_image_build: 
#' Builds docker image for the aws-ecr
#' @description faasr_register_workflow_aws_lambda_function_build:
#' Create aws lambda functions
#' @description check_lambda_exists:
#' create or update lambda function
#' @description execute_command_with_retry:
#' check if a Lambda function exists
#' 
#' @param faasr_register_workflow_aws_lambda: "faasr" for list of the json
#' @param faasr_register_workflow_lambda_function_lists: "faasr" for list of the json
#' @param faasr_register_workflow_lambda_function_image: "faasr" for list of the json
#' @param faasr_register_workflow_aws_ecr_build: "faasr" for list of the json
#' @param faasr_register_workflow_aws_lambda_role_create: "faasr" for list of the json
#' @param faasr_register_workflow_aws_ecr_image_build: "function_image_list" for  the list of imagename:image url pairs,
#' aws_ecr_repo_name for the string
#' @param faasr_register_workflow_aws_lambda_function_build: "lambda_function_info" for the list of image and actionname pairs,
#' "function_image_list" for the list of imagename:image url pairs, "aws_lambda_role_name" for the string
#' @param check_lambda_exists: function_name for the string
#' @param execute_command_with_retry: "command" for the string
#' 
#' @return faasr_register_workflow_aws_lambda: "faasr" for list of the json
#' @return faasr_register_workflow_lambda_function_lists: "lambda_function_info" for the list of 
#' actionname and "create" or "update" pairs.
#' @return faasr_register_workflow_lambda_function_image: "function_image_list" for the list of
#' actionname:image pairs
#' @return faasr_register_workflow_aws_ecr_build: "aws_ecr_repo_name" for the string
#' @return faasr_register_workflow_aws_lambda_role_create: "aws_lambda_role_name" for the string
#' @return faasr_register_workflow_aws_ecr_image_build: "function_image_list" for the list of 
#' imagename:image url pairs
#' @return check_lambda_exists: boolean value
#' @return execute_command_with_retry: boolean value
#' 


# specify global variables
faasr_defined_image_lambda <- "145342739029.dkr.ecr.us-east-1.amazonaws.com/aws-lambda-tidyverse:latest"
# faasr_defined_image_lambda <- basic_ld_image

faasr_register_workflow_aws_lambda <- function(faasr, cred){

  # get region from faasr
  for (faas in names(faasr$ComputeServers)){
    if(faasr$ComputeServers[[faas]]$FaaSType == "Lambda"){
      aws_region <- faasr$ComputeServers[[faas]]$Region
      break
    }
  }
  
  
  # get aws lambda function list
  lambda_function_info <- faasr_register_workflow_lambda_function_lists(faasr, cred, aws_region)
  if (length(lambda_function_info)==0){
    return(FALSE)
  }
  # get aws lambda function image list
  function_image_list <- faasr_register_workflow_lambda_function_image(faasr)
  # build aws ECR repo
  aws_ecr_repo_name <- faasr_register_workflow_aws_ecr_build(faasr, cred, aws_region)
  #create aws lambda function role
  aws_lambda_role_name <- faasr_register_workflow_aws_lambda_role_create(faasr, cred, aws_region)
  # upload ECR repo image
  function_image_list_update <- faasr_register_workflow_aws_ecr_image_build(function_image_list,aws_ecr_repo_name, cred, aws_region)
  # create aws lambda functions
  faasr_register_workflow_aws_lambda_function_build(lambda_function_info, function_image_list_update, aws_lambda_role_name, cred, aws_region)
  
  return(faasr)
}

# Get aws lambda function list
faasr_register_workflow_lambda_function_lists <- function(faasr,cred, aws_region){
  
  function_list <- faasr$FunctionList
  compute_servers <- faasr$ComputeServers
  
  # initialize the list
  lambda_function_info <- list()
  
  # iterate over the functions
  for (func_name in names(function_list)) {
    
    func <- function_list[[func_name]]
    server_name <- func$FaaSServer
    server_type <- compute_servers[[server_name]]$FaaSType
    action_name <- func_name
    
    # check if the function server type is Lambda
    if (server_type == "Lambda") {
      if(!action_name %in% names(lambda_function_info)){
        lambda_function_info[[action_name]]$image <- action_name
      }
    }
  }
  for(action_name in names(lambda_function_info)){
    if(check_lambda_exists(action_name, cred, aws_region)){
      cat("\n\n[faasr_msg] lambda function -- ", action_name, "already exists.\n")
      cat("[faasr_msg] Do you want to update it?[y/n]\n")
      while(TRUE) {
        check <- readline()
        if(check == "y"){
          lambda_function_info[[action_name]]$action <- "update"
          break
        } else if(check == 'n'){
          cat("\n[faasr_msg] stop the script\n")
          stop()
        }else {
          cat("Enter \"y\" or \"n\": ")
        }
      }
    } else {
      lambda_function_info[[action_name]]$action <- "create"
    }
  }
  
  return (lambda_function_info)
}

# Get aws lambda function image list
faasr_register_workflow_lambda_function_image <- function(faasr){
  
  function_list <- faasr$FunctionList
  compute_servers <- faasr$ComputeServers
  
  # initialize the lists
  function_image_list <- list()
  
  # iterate over the functions
  for (func_name in names(function_list)) {
    func <- function_list[[func_name]]
    server_name <- func$FaaSServer
    server_type <- compute_servers[[server_name]]$FaaSType
    action_name <- func_name
    # check if the function server type is Lambda
    if (server_type == "Lambda") {
      # add the {action_name:image} pair to the function_image_list, if it's not present
      if (!action_name %in% names(function_image_list)) {
        action_name_value <- faasr$ActionContainers[[action_name]]
        if(length(action_name_value)== 0 || action_name_value == ""){
          image_path <- faasr_defined_image_lambda
        } else {
          image_path <- faasr$ActionContainers[[action_name]]
        }
        function_image_list[[action_name]] <- image_path
      }
    }
  }
  return (function_image_list)
}


# Build aws ecr repository
faasr_register_workflow_aws_ecr_build <- function(faasr, cred, aws_region){
  sts_instance <- paws::sts(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )

  aws_account_id <- sts_instance$get_caller_identity()$Account
  # print("aws_account_id")
  # print(aws_account_id)
  
  
  # ask user to acquire ecr repo name
  cat("\n\n[faasr_msg]The ECR Repositery you want to create or use: [for example: lambda-script]\n")
  aws_ecr_repo_name <- NULL
  while(TRUE) {
    aws_ecr_repo_name <- readline()
    if(aws_ecr_repo_name != ""){
      break
    }
  }
  
  # create AWS ECR repo
  # check if this repo has exist

  ecr_instance <- paws::ecr(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  
  # Using tryCatch to handle exceptions
  check_result <- tryCatch({
    result <- ecr_instance$describe_repositories(registryId = aws_account_id, repositoryNames = aws_ecr_repo_name)
  }, error = function(e) {
    # Check if the error is a RepositoryNotFoundException
    if(grepl("HTTP 400", e$message)) {
      return(400)
    } else {
      stop(e)  # Re-throw other errors
    }
  })
  # check response

  if(length(check_result)== 1 && check_result == 400){
    cat("\n[faasr_msg] will create new ecr repo ", aws_ecr_repo_name, "\n")

    create_result <- ecr_instance$create_repository(registryId = aws_account_id, repositoryName = aws_ecr_repo_name)

    print(create_result)
  }else{
    print(paste0("Repository ", aws_ecr_repo_name, " already exists."))
  }
  

  return (aws_ecr_repo_name)
}

# create lambda role
faasr_register_workflow_aws_lambda_role_create <- function(faasr, cred, aws_region){
  
  # ask user to acuqire aws lambda role name
  cat("\n[faasr_msg] The name of AWS lambda role that you want to create or use: [for example: faasr-lambda]\n")
  aws_lambda_role_name <- NULL
  while(TRUE) {
    aws_lambda_role_name <- readline()
    if(aws_lambda_role_name != ""){
      break
    }
  }
  # aws command to list all roles
  iam_instance <- paws::iam(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  roles_output_json <- iam_instance$list_roles()
  
  #parsed_roles <- jsonlite::fromJSON(roles_output_json)
  list_role_names <- sapply(roles_output_json$Roles, function(x) x$RoleName)
  # check this role name exist
  role_exists <- aws_lambda_role_name %in% list_role_names
  # if the role does not exist, create it
  if (!role_exists) {
    # define the trust policy
    trust_policy <- '{"Version": "2012-10-17","Statement": [{ "Effect": "Allow", "Principal": {"Service": "lambda.amazonaws.com"}, "Action": "sts:AssumeRole"}]}'
    
    # aws command to create role with the trust policy
    create_role_output <- iam_instance$create_role(RoleName = aws_lambda_role_name, AssumeRolePolicyDocument = trust_policy)
    
    print(paste("Created role:", aws_lambda_role_name))
    
    # attach policy to role
    iam_instance$attach_role_policy(RoleName = aws_lambda_role_name, PolicyArn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole")
    print("attach AWSLambdaBasicExecutionRole policy to new role")
    
  } else {
    print(paste("Role", aws_lambda_role_name, "already exists."))
  }
  
  return (aws_lambda_role_name)
}

# Build docker image
faasr_register_workflow_aws_ecr_image_build <- function(function_image_list,aws_ecr_repo_name, cred, aws_region){

  sts_instance <- paws::sts(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  aws_account_id <- sts_instance$get_caller_identity()$Account
  
  docker_aws_connected <- FALSE
  
  # upload images to aws ecr repo
  for(image_name in names(function_image_list)){
    source_img_url <- function_image_list[[image_name]]
    # check if image provided by client or faasr, if provided by client, then pull from docker hub, if provided by faasr, then use the image name
    if(source_img_url != faasr_defined_image_lambda){
      if(!docker_aws_connected){
      # connect aws ECR and Docker
        ecr_docker_connect_cmd <- 
          paste0("aws ecr get-login-password --region ", aws_region, " | docker login --username AWS --password-stdin ",aws_account_id,".dkr.ecr.",aws_region,".amazonaws.com")
        pipe_conn <- pipe(ecr_docker_connect_cmd)
        pipe_output <- readLines(pipe_conn)
        #pipe_output <- readline(pipe_conn)
        close(pipe_conn)
        docker_aws_connected <- TRUE
      }
      #pull from docker hub
      system(paste0("docker pull ", source_img_url))
      # give it a tag for ecr repo
      aws_ecr_img_url <- paste0(aws_account_id, ".dkr.ecr.", aws_region, ".amazonaws.com/", aws_ecr_repo_name, ":",image_name)
      
      system(paste0("docker tag ", source_img_url, " ", aws_ecr_img_url))
      
      # push an image to Amazon ECR
      system(paste0("docker push ", aws_ecr_img_url))
      function_image_list[[image_name]] <- aws_ecr_img_url
    }else{
      function_image_list[[image_name]] <- source_img_url
    }
  }
  
  return (function_image_list)
}

# Create aws lambda functions
faasr_register_workflow_aws_lambda_function_build <- function(lambda_function_info, function_image_list, aws_lambda_role_name, cred, aws_region){
  sts_instance <- paws::sts(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  aws_account_id <- sts_instance$get_caller_identity()$Account
  
  # set configuration for new lambda function
  # ask user to specify the function timeout and memory size
  has_create <- any(sapply(lambda_function_info, function(x) x$action == "create"))
  #if(has_create){
  cat("\n[faasr_msg] Set lambda function timeout(sec) [60 to 900]:\n")
  while(TRUE) {
    aws_lambda_timeout <- readline()
    timeout_numeric_input <- suppressWarnings(as.numeric(aws_lambda_timeout))
    # Check if the input is numeric and between 60 and 900
    if(aws_lambda_timeout != "" && !is.na(timeout_numeric_input) && timeout_numeric_input >= 60 && timeout_numeric_input <= 900){
      break
    } else {
      cat("[faasr_msg] Invalid input. Please enter a numeric value between 60 and 900:\n")
    }
  }
  # convert the input to a numeric value
  aws_lambda_timeout <- as.numeric(aws_lambda_timeout)
  
  cat("\n[faasr_msg] Set lambda function memory size(MB) [256 to 10240]:\n")
  while(TRUE) {
    aws_lambda_memory <- readline()
    memory_numeric_input <- suppressWarnings(as.numeric(aws_lambda_memory))
    # Check if the input is numeric and between 60 and 900
    if(aws_lambda_memory != "" && !is.na(memory_numeric_input) && memory_numeric_input >= 256 && memory_numeric_input <= 10240){
      break
    } else {
      cat("[faasr_msg] Invalid input. Please enter a numeric value between 256 and 10240:\n")
    }
  }
  # convert the input to a numeric value
  aws_lambda_memory <- as.numeric(aws_lambda_memory)
  #}
  
  #build lambda role arn
  lambda_role_arn <- paste0("arn:aws:iam::",aws_account_id,":role/", aws_lambda_role_name)

  # build paws lambda instance
  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  
  #update lambda function configuration if needed
  for (function_name in names(lambda_function_info)){
    
    function_image_name <- lambda_function_info[[function_name]]$image
    function_image_url <- function_image_list[[function_image_name]]
    
    
    if(lambda_function_info[[function_name]]$action == "update"){
      cat("\n[faasr_msg] Will update lambda function configuration", function_name, "\n")
      lambda_instance$update_function_configuration(FunctionName = function_name, Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)
      #print(return_status)
      
    }
  }
  
  # create or update lambda function
  for (function_name in names(lambda_function_info)){
    
    function_image_name <- lambda_function_info[[function_name]]$image
    function_image_url <- function_image_list[[function_image_name]]

    if(lambda_function_info[[function_name]]$action == "update"){
      cat("\n[faasr_msg] Will update lambda function image", function_name, "\n")
      update_lambda_command <- paste0("aws lambda update-function-code --function-name ",function_name," --image-uri ", function_image_url)
      print(update_lambda_command)
      execute_command_with_retry(function_name, function_image_url, cred, aws_region)
      
    } else if(lambda_function_info[[function_name]]$action == "create"){
      cat("\n[faasr_msg] Will create lambda function", function_name, "\n")
      # create lambda function
      lambda_instance$create_function(FunctionName = function_name, PackageType = "Image", Code = list(ImageUri = function_image_url), Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)  

    }
  }
}


# check if a Lambda function exists
check_lambda_exists <- function(function_name, cred, aws_region) {
  # aws command check if a function exist

  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  check_result <- tryCatch({
    result <- lambda_instance$get_function(FunctionName = function_name)
  }, error = function(e) {
    # Check if the error is a RepositoryNotFoundException
    if(grepl("HTTP 404", e$message)) {
      return(404)
    } else {
      stop(e)
    }
  })
  # check if the function exists
  if (length(check_result) == 1 && check_result == 404) {
    # function does not exist
    return(FALSE)
  } else {
    # function exists
    return(TRUE)
  }
}

# check if aws command run successfully, and retry

execute_command_with_retry <- function(function_name, function_image_url, cred, aws_region, max_retries = 3, sleep_seconds = 3) {

  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )

  print("update function code: execute_command_with_retry")
  

  for (i in 1:max_retries) {
    check_result <- tryCatch({
     result <- lambda_instance$update_function_code(FunctionName = function_name, ImageUri = function_image_url)
     
      }, error = function(e) {
        # Check if the error is a RepositoryNotFoundException
        if(grepl("HTTP 409", e$message)) {
          return(409)
        } else {
          stop(e)
        }
      })
    # check the status code to see if command failed
    if (length(check_result)== 1 && check_result == 409) {
      cat("[faasr_msg] Command failed with status code 409. Retrying in", sleep_seconds, "seconds...\n")
    } else {
        cat("[faasr_msg] Update is in progress or completed successfully.\n")
        return(TRUE)

    }
    
    # Pause before retrying
    Sys.sleep(sleep_seconds)
  }
  cat("[faasr_msg] Max retries reached. Exiting.\n")
  return(FALSE)
}

# set workflow timer for lambda
faasr_set_workflow_timer_ld <- function(faasr, cred, target, cron, unset=FALSE){

  # get region from faasr
  for (faas in names(faasr$ComputeServers)){
    if(faasr$ComputeServers[[faas]]$FaaSType == "Lambda"){
      aws_region <- faasr$ComputeServers[[faas]]$Region
      break
    }
  }
  
  faasr_w_cred <- faasr_replace_values(faasr, cred)
  faasr_payload_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox = TRUE)
  
    sts_instance <- paws::sts(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )

  aws_account_id <- sts_instance$get_caller_identity()$Account
  
  # get the lambda function name
  lambda_function_name <- faasr$FunctionInvoke

  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )

  eventbridge_instance <- paws::eventbridge(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=cred$lambda_a_ACCESS_KEY,
          secret_access_key=cred$lambda_a_SECRET_KEY,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  
  if (unset==TRUE){

    event_rule_name <- paste0('FaaSr-scheduled-cron-rule-', lambda_function_name)

    # remove event permission from lambda
    unset_lambda_permission_result <- lambda_instance$remove_permission(FunctionName = lambda_function_name, StatementId = event_rule_name)
    
    # remove target from event
    unset_event_target_result <- eventbridge_instance$remove_targets(Rule = event_rule_name, Ids = "1")
    
    # remove event rule
    unset_events_put_rules_result <- eventbridge_instance$delete_rule(Name = event_rule_name)
    
  } else{
    
    # set event rules
    event_cron_rule <- cron
    cat("event schedule: cron(",  event_cron_rule, ")\n")
    #event_cron_rule <- "cron(0/5 * * * ? *)" # every 5 minute
    
    # set event rule name based on lambda function name
    event_rule_name <- paste0('FaaSr-scheduled-cron-rule-', lambda_function_name)

    set_events_put_rules_result <- eventbridge_instance$put_rule(Name = event_rule_name, ScheduleExpression = paste0("cron(",  event_cron_rule, ")"))

    
    # add permission to lambda

    set_lambda_permission_result <- 0
    tryCatch({
      lambda_instance$add_permission(FunctionName = lambda_function_name, StatementId = event_rule_name, Action = "lambda:InvokeFunction", Principal = "events.amazonaws.com", SourceArn = paste0("arn:aws:events:", aws_region, ":", aws_account_id, ":rule/",event_rule_name))
    }, error = function(e) {
      # the error is a RepositoryNotFoundException
      set_lambda_permission_result <- 409
    })

    # check if the permission statement already exist
    if (length(set_lambda_permission_result) == 1  && set_lambda_permission_result == 409){
      print("permission statement already exist")
    }
    
    # set event target and input payload
    lambda_function_arn <- paste0('arn:aws:lambda:', aws_region, ':', aws_account_id, ':function:', lambda_function_name)
    # Construct the content for faasr_lambda_event_targets.json
    targets <- list(
      list(
        Id = "1",
        Arn = lambda_function_arn,
        Input = faasr_payload_json
      )
    )
    
    # Convert the targets list to JSON
    targets_json <- jsonlite::toJSON(targets, pretty = TRUE, auto_unbox = TRUE)
    
    # Write faasr_lambda_event_targets.json to file
    write(targets_json, file = "faasr_lambda_event_targets.json")
    
    # set event target with the faasr_lambda_event_targets.json

    set_lambda_timer_result <- eventbridge_instance$put_targets(Rule = event_rule_name, Targets = targets)
    
    
    file.remove("faasr_lambda_event_targets.json")
  }
  
}


                           
