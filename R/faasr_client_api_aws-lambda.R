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


faasr_register_workflow_aws_lambda <- function(faasr, cred){
  
  
  # get aws lambda function list
  lambda_function_info <- faasr_register_workflow_lambda_function_lists(faasr)
  if (length(lambda_function_info)==0){
    return(FALSE)
  }
  # get aws lambda function image list
  function_image_list <- faasr_register_workflow_lambda_function_image(faasr)
  # build aws ECR repo
  aws_ecr_repo_name <- faasr_register_workflow_aws_ecr_build(faasr)
  #create aws lambda function role
  aws_lambda_role_name <- faasr_register_workflow_aws_lambda_role_create(faasr)
  # upload ECR repo image
  function_image_list_update <- faasr_register_workflow_aws_ecr_image_build(function_image_list,aws_ecr_repo_name)
  # create aws lambda functions
  faasr_register_workflow_aws_lambda_function_build(lambda_function_info, function_image_list_update, aws_lambda_role_name)
  
  return(faasr)
}

# Get aws lambda function list
faasr_register_workflow_lambda_function_lists <- function(faasr){
  
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
    if(check_lambda_exists(action_name)){
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
          image_path <- "faasr/aws-lambda-tidyverse"
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
faasr_register_workflow_aws_ecr_build <- function(faasr){
  # get the AWS account ID
  get_account_id_command <- "aws sts get-caller-identity --query Account --output text"
  aws_account_id <- system(get_account_id_command, intern = TRUE)
  
  # get the AWS region
  get_region_command <- "aws configure get region"
  aws_region <- system(get_region_command, intern = TRUE)
  
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
  check_command <- paste0("aws ecr describe-repositories --repository-names ", aws_ecr_repo_name, " --region ", aws_region)
  check_result <- system(check_command, intern = TRUE)
  # access the status code
  status_code <- attr(check_result, "status")
  # if the repository does not exist, create it
  if (!is.null(status_code) && status_code == 254) {
    
    cat("\n[faasr_msg] will create new ecr repo ", aws_ecr_repo_name, "\n")
    create_command <- paste0("aws ecr create-repository --repository-name ", aws_ecr_repo_name, " --region ", aws_region)
    create_result <- system(create_command, intern = TRUE)
    print(create_result)
  } else {
    print(paste0("Repository ", aws_ecr_repo_name, " already exists."))
  }
  return (aws_ecr_repo_name)
}

# create lambda role
faasr_register_workflow_aws_lambda_role_create <- function(faasr){
  
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
  list_roles_command <- "aws iam list-roles"
  roles_output_json <- system(list_roles_command, intern = TRUE, ignore.stderr = TRUE)
  
  parsed_roles <- jsonlite::fromJSON(paste(roles_output_json, collapse = ""))
  
  # check this role name exist
  role_exists <- aws_lambda_role_name %in% parsed_roles$Roles$RoleName
  
  # if the role does not exist, create it
  if (!role_exists) {
    # define the trust policy
    trust_policy <- '{"Version": "2012-10-17","Statement": [{ "Effect": "Allow", "Principal": {"Service": "lambda.amazonaws.com"}, "Action": "sts:AssumeRole"}]}'
    
    # aws command to create role with the trust policy
    create_role_command <- paste0('aws iam create-role --role-name ', aws_lambda_role_name, ' --assume-role-policy-document ', shQuote(trust_policy))
    
    create_role_output <- system(create_role_command, intern = TRUE)
    
    print(paste("Created role:", aws_lambda_role_name))
    
    # attach policy to role
    attach_policy_command <- paste0("aws iam attach-role-policy --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole --role-name ", aws_lambda_role_name)
    
    system(attach_policy_command, intern = TRUE)
    print("attach AWSLambdaBasicExecutionRole policy to new role")
    
  } else {
    print(paste("Role", aws_lambda_role_name, "already exists."))
  }
  
  return (aws_lambda_role_name)
}

# Build docker image
faasr_register_workflow_aws_ecr_image_build <- function(function_image_list,aws_ecr_repo_name ){
  
  # get the AWS account ID
  get_account_id_command <- "aws sts get-caller-identity --query Account --output text"
  aws_account_id <- system(get_account_id_command, intern = TRUE)
  
  # get the AWS region
  get_region_command <- "aws configure get region"
  aws_region <- system(get_region_command, intern = TRUE)
  
  # connect aws ECR and Docker
  ecr_docker_connect_cmd <- 
    paste0("aws ecr get-login-password --region ", aws_region, " | docker login --username AWS --password-stdin ",aws_account_id,".dkr.ecr.",aws_region,".amazonaws.com")
  pipe_conn <- pipe(ecr_docker_connect_cmd)
  pipe_output <- readLines(pipe_conn)
  #pipe_output <- readline(pipe_conn)
  close(pipe_conn)
  
  # upload images to aws ecr repo
  for(image_name in names(function_image_list)){
    source_img_url <- function_image_list[[image_name]]
    #pull from docker hub
    system(paste0("docker pull ", source_img_url))
    # give it a tag for ecr repo
    aws_ecr_img_url <- paste0(aws_account_id, ".dkr.ecr.", aws_region, ".amazonaws.com/", aws_ecr_repo_name, ":",image_name)
    
    system(paste0("docker tag ", source_img_url, " ", aws_ecr_img_url))
    
    # push an image to Amazon ECR
    system(paste0("docker push ", aws_ecr_img_url))
    function_image_list[[image_name]] <- aws_ecr_img_url
  }
  
  return (function_image_list)
}

# Create aws lambda functions
faasr_register_workflow_aws_lambda_function_build <- function(lambda_function_info, function_image_list, aws_lambda_role_name){
  
  # get the AWS account ID
  get_account_id_command <- "aws sts get-caller-identity --query Account --output text"
  aws_account_id <- system(get_account_id_command, intern = TRUE)
  
  # get the AWS region
  get_region_command <- "aws configure get region"
  aws_region <- system(get_region_command, intern = TRUE)
  
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
  
  #update lambda function configuration if needed
  for (function_name in names(lambda_function_info)){
    
    function_image_name <- lambda_function_info[[function_name]]$image
    function_image_url <- function_image_list[[function_image_name]]
    
    if(lambda_function_info[[function_name]]$action == "update"){
      cat("\n[faasr_msg] Will update lambda function configuration", function_name, "\n")
      update_lambda__config_command <- paste0("aws lambda update-function-configuration --function-name ",function_name," --role ",lambda_role_arn," --timeout ",aws_lambda_timeout," --memory-size ",aws_lambda_memory)
      print(update_lambda__config_command)
      return_status <- system(update_lambda__config_command, intern = TRUE)
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
      #system(update_lambda_command, intern = TRUE)
      execute_command_with_retry(update_lambda_command)
      
    } else if(lambda_function_info[[function_name]]$action == "create"){
      cat("\n[faasr_msg] Will create lambda function", function_name, "\n")
      create_lambda_command <- paste0("aws lambda create-function --region ",aws_region," --function-name ",function_name," --package-type Image --code ImageUri=",function_image_url," --role ",lambda_role_arn ," --timeout ",aws_lambda_timeout," --memory-size ",aws_lambda_memory)
      print(create_lambda_command)
      system(create_lambda_command, intern = TRUE)
    }
  }
}


# check if a Lambda function exists
check_lambda_exists <- function(function_name) {
  # aws command check if a function exist
  check_command <- paste0("aws lambda get-function --function-name ", function_name)
  check_result <- system(check_command, intern = TRUE, ignore.stderr = TRUE)
  
  if (length(check_result) == 0) {
    # function does not exist
    return(FALSE)
  } else {
    # function exists
    return(TRUE)
  }
}

# check if aws command run successfully, and retry
execute_command_with_retry <- function(command, max_retries = 3, sleep_seconds = 3) {
  
  for (i in 1:max_retries) {
    command_output <- system(command, intern = TRUE)
    status_code <- attr(command_output, "status")
    
    # check the status code to see if command failed
    if (!is.null(status_code) && status_code == 254) {
      cat("[faasr_msg] Command failed with status code 254. Retrying in", sleep_seconds, "seconds...\n")
    } else {
      
      json_output <- try(jsonlite::fromJSON(paste(command_output, collapse = "")), silent = TRUE)
      
      if (!inherits(json_output, "try-error") && json_output$LastUpdateStatus %in% c("InProgress", "Complete")) {
        cat("[faasr_msg] Update is in progress or completed successfully.\n")
        return(TRUE)
      } else {
        cat("[faasr_msg] LastUpdateStatus is not as expected. Retrying in", sleep_seconds, "seconds...\n")
      }
    }
    
    # Pause before retrying
    Sys.sleep(sleep_seconds)
  }
  cat("[faasr_msg] Max retries reached. Exiting.\n")
  return(FALSE)
}

# set workflow timer for lambda
faasr_set_workflow_timer_ld <- function(faasr, cred, target, cron, unset=FALSE){
  
  faasr_w_cred <- FaaSr::faasr_replace_values(faasr, cred)
  faasr_payload_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox = TRUE)
  
  # get the AWS account ID
  get_account_id_command <- "aws sts get-caller-identity --query Account --output text"
  aws_account_id <- system(get_account_id_command, intern = TRUE)
  
  # get the AWS region
  aws_region <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$Region
  
  # get the lambda function name
  lambda_function_name <- faasr$FunctionInvoke
  if (unset==TRUE){
    
  } else{
    
    # set event rules
    event_cron_rule <- cron
    cat("event schedule: cron(",  event_cron_rule, ")\n")
    #event_cron_rule <- "cron(0/5 * * * ? *)" # every 5 minute
    
    set_events_put_rules_command <- paste0('aws events put-rule --name FaaSr-scheduled-cron-rule --schedule-expression "cron(',  event_cron_rule, ')"')
    set_events_put_rules_result <- system(set_events_put_rules_command, intern = TRUE)
    
    
    # add permission to lambda
    set_lambda_permission_command <- paste0('aws lambda add-permission --function-name ', lambda_function_name, ' --statement-id FaaSr-scheduled-cron-rule --action "lambda:InvokeFunction" --principal events.amazonaws.com --source-arn arn:aws:events:', aws_region, ':', aws_account_id, ':rule/FaaSr-scheduled-cron-rule')
    set_lambda_permission_result <- system(set_lambda_permission_command, intern = TRUE)
    
    
    # access the status code
    status_code <- attr(set_lambda_permission_result, "status")
    # check if the permission statement already exist
    if (!is.null(status_code) && status_code == 254){
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
    set_lambda_timer_command <- "aws events put-targets --rule FaaSr-scheduled-cron-rule --targets file://faasr_lambda_event_targets.json"
    set_lambda_timer_result <- system(set_lambda_timer_command, intern = TRUE)
    
    file.remove("faasr_lambda_event_targets.json")
  }
  
}


                           
