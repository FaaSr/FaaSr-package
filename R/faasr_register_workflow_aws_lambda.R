#' @title Interactive function to register a workflow with AWS Lambda Function
#' @description This script automates the process of using AWS command-line interface tools to register
#'              the various actions that are part of a user's workflow with AWS Lambda Functions
#'              The script depends on the user having the AWS command-line version 2 tool installed  and configured in their computer:
#'                 https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
#'              As well as the Docker CLI installed  and configured in their computer:
#'                 https://docs.docker.com/get-docker/
#'              The script expects a command-line argument that is the name of a FaaSr-compliant JSON configuration
#'              If successful, the script registers all the actions declared in the workflow bound to the
#'              Docker containers also declared in the workflow
#' @param payload_file name of the JSON payload file



faasr_register_workflow_aws_lambda <- function(payload_file){

    # receive the user's json file as an argument
    faasr <- jsonlite::fromJSON(payload_file)

    # get aws lambda function list
    lambda_function_info <- faasr_register_workflow_lambda_function_lists(faasr)
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
        action_name <- func$Actionname

        # check if the function server type is Lambda
        if (server_type == "Lambda") {

            if(check_lambda_exists(func_name)){
                cat("\nlambda function -- ", func_name, "already exists.\n")
                cat("Do you want to update it?[y/n]\n")
                while(TRUE) {
                    # check <- readLines(con = "stdin", 1)
                    check <- readline()
                    if(check == "y"){
                    lambda_function_info[[func_name]]$action <- "update"
                    break
                    } else if(check == 'n'){
                    cat("\nstop the script\n")
                    stop()
                    }else {
                    cat("Enter \"y\" or \"n\": ")
                    }
                }
            } else {
                lambda_function_info[[func_name]]$action <- "create"
            }
            # add the function to the lambda function info list
            lambda_function_info[[func_name]]$image <- action_name
            
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
        action_name <- func$Actionname
        # check if the function server type is Lambda
        if (server_type == "Lambda") {

            # add the {action_name:image} pair to the function_image_list, if it's not present
            if (!action_name %in% names(function_image_list)) {
                # if "Actionname" is "fassr", then use faasr provided image--faasr/aws-lambda-tidyverse,
                # otherwise use user provided image
                # if(action_name == "faasr"){
                #     image_path <- "faasr/aws-lambda-tidyverse"
                # } 
                action_name_value <- faasr$ActionContainers[[action_name]]
                if(length(action_name_value)== 0 || action_name_value == ""){
                    image_path <- "faasr/aws-lambda-tidyverse"
                }
                else {
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
    cat("\nThe ECR Repositery you want to create or use: [for example: lambda-script]\n")
    aws_ecr_repo_name <- NULL
    while(TRUE) {
        # aws_ecr_repo_name <- readLines(con = "stdin", 1)
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

        cat("\nwill create new ecr repo ", aws_ecr_repo_name, "\n")
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
    cat("\nThe name of AWS lambda role that you want to create or use: [for example: faasr-lambda]\n")
    aws_lambda_role_name <- NULL
    while(TRUE) {
        #aws_lambda_role_name <- readLines(con = "stdin", 1)
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
        cat("\nSet lambda function timeout(sec) [60 to 900]:\n")
        while(TRUE) {
            #aws_lambda_timeout <- readLines(con = "stdin", 1)
            aws_lambda_timeout <- readline()
            timeout_numeric_input <- suppressWarnings(as.numeric(aws_lambda_timeout))
            # Check if the input is numeric and between 60 and 900
            if(aws_lambda_timeout != "" && !is.na(timeout_numeric_input) && timeout_numeric_input >= 60 && timeout_numeric_input <= 900){
            break
            } else {
            cat("Invalid input. Please enter a numeric value between 60 and 900:\n")
            }
        }
        # convert the input to a numeric value
        aws_lambda_timeout <- as.numeric(aws_lambda_timeout)

        cat("\nSet lambda function memory size(MB) [256 to 10240]:\n")
        while(TRUE) {
            #aws_lambda_memory <- readLines(con = "stdin", 1)
            aws_lambda_memory <- readline()
            memory_numeric_input <- suppressWarnings(as.numeric(aws_lambda_memory))
            # Check if the input is numeric and between 60 and 900
            if(aws_lambda_memory != "" && !is.na(memory_numeric_input) && memory_numeric_input >= 256 && memory_numeric_input <= 10240){
            break
            } else {
            cat("Invalid input. Please enter a numeric value between 256 and 10240:\n")
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
            cat("\nWill update lambda function configuration", function_name, "\n")
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
            cat("\nWill update lambda function image", function_name, "\n")
            update_lambda_command <- paste0("aws lambda update-function-code --function-name ",function_name," --image-uri ", function_image_url)
            print(update_lambda_command)
            #system(update_lambda_command, intern = TRUE)
            execute_command_with_retry(update_lambda_command)

        } else if(lambda_function_info[[function_name]]$action == "create"){
            cat("\nWill create lambda function", function_name, "\n")
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
      cat("Command failed with status code 254. Retrying in", sleep_seconds, "seconds...\n")
    } else {
      
      json_output <- try(jsonlite::fromJSON(paste(command_output, collapse = "")), silent = TRUE)

      if (!inherits(json_output, "try-error") && json_output$LastUpdateStatus %in% c("InProgress", "Complete")) {
        cat("Update is in progress or completed successfully.\n")
        return(TRUE)
      } else {
        cat("LastUpdateStatus is not as expected. Retrying in", sleep_seconds, "seconds...\n")
      }
    }
    
    # Pause before retrying
    Sys.sleep(sleep_seconds)
  }
  cat("Max retries reached. Exiting.\n")
  return(FALSE)
}

