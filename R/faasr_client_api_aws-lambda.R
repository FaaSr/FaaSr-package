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
  # collect lambda server information
  lambda_server_info <- list()
  aws_account_checked <- FALSE
  for (faas in names(faasr$ComputeServers)){
    if(faasr$ComputeServers[[faas]]$FaaSType == "Lambda"){
      aws_region <- faasr$ComputeServers[[faas]]$Region
      # get access key and secret key from cred
      access_key_name <- paste0(faas, "_ACCESS_KEY")
      secret_key_name <- paste0(faas, "_SECRET_KEY")
      aws_access_key <- cred[[access_key_name]]
      aws_secret_key <- cred[[secret_key_name]]
      lambda_server_info[[faas]]$aws_access_key <- aws_access_key
      lambda_server_info[[faas]]$aws_secret_key <- aws_secret_key
      lambda_server_info[[faas]]$aws_region <- aws_region
      
      if(!aws_account_checked){
        # get aws account id
        sts_instance <- paws::sts(
          config=list(
            credentials=list(
              creds=list(
                access_key_id=aws_access_key,
                secret_access_key=aws_secret_key,
                session_token=""
              )
            ),
            region=aws_region
          )
        )
        aws_account_id <- sts_instance$get_caller_identity()$Account
        aws_account_checked <- TRUE
      }

      lambda_server_info[[faas]]$aws_account_id <- aws_account_id

    }
  }
  
  #print(lambda_server_info)


  
  
  # get aws lambda function list
  lambda_function_info <- faasr_register_workflow_lambda_function_lists(faasr, cred, lambda_server_info)
  if (length(lambda_function_info)==0){
    return("")
  }

  # get aws lambda function image list
  function_image_list <- faasr_register_workflow_lambda_function_image(faasr, lambda_server_info)

  #print(function_image_list)


  #create aws lambda function role
  aws_lambda_role_name <- faasr_register_workflow_aws_lambda_role_create(faasr, cred, lambda_server_info)


  # create aws lambda functions
  faasr_register_workflow_aws_lambda_function_build(faasr, lambda_function_info, function_image_list, aws_lambda_role_name, cred, lambda_server_info)
  
  return(faasr)
}

# Get aws lambda function list
faasr_register_workflow_lambda_function_lists <- function(faasr,cred, lambda_server_info){
  
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
    server_name <- function_list[[action_name]]$FaaSServer
    current_lambda_server_info <- lambda_server_info[[server_name]]

    if(check_lambda_exists(action_name, cred, current_lambda_server_info)){
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
faasr_register_workflow_lambda_function_image <- function(faasr, lambda_server_info){
  
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
          # user don't provide image, use the default image
          # get function region
          server_region <- lambda_server_info[[server_name]]$aws_region
          current_basic_ld_image <- paste0(basic_ld_image_account, server_region, basic_ld_image_tag)
          image_path <- current_basic_ld_image
        } else {
          user_image_url <- faasr$ActionContainers[[action_name]]
          # check if user provided image exists, if not, return false then stop processing
          if(!check_user_image_exist(faasr, action_name, server_name, user_image_url, lambda_server_info[[server_name]])){
            cat("\n[faasr_msg] stop the script\n")
            stop()
          }
          image_path <- user_image_url
        }
        function_image_list[[action_name]] <- image_path
      }
    }
  }
  return (function_image_list)
}

# check if user provided image exists, if not, return false then stop processing
check_user_image_exist <- function(faasr, action_name, server_name, user_image_url, current_lambda_server_info){
  # Split by '/' and then by ':'
  splited_img <- unlist(strsplit(user_image_url, "/"))
  splited_image_part <- splited_img[2]
  
  repo_tag_part <- unlist(strsplit(splited_image_part, ":"))
  repo_name <- repo_tag_part[1]
  image_tag <- repo_tag_part[2]

  ecr_instance <- paws::ecr(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=current_lambda_server_info$aws_access_key,
          secret_access_key=current_lambda_server_info$aws_secret_key,
          session_token=""
        )
      ),
      region=current_lambda_server_info$aws_region
    )
  )
  
  # check if the image exists
  check_result <- tryCatch({
    result <- ecr_instance$describe_images(repositoryName = repo_name, imageIds = list(imageTag = image_tag))
  }, error = function(e) {
    # Check if the error is a RepositoryNotFoundException
    if(grepl("HTTP 400", e$message)) {
      return(400)
    } else {
      stop(e)  # Re-throw other errors
    }
  })


  # check if the repo exists
  if (length(check_result) == 1 && check_result == 400) {
    # repo does not exist
    cat("\n[faasr_msg] repo name ", repo_name, " not exist, please check\n")
    return(FALSE)
  } else {
    # repo exists then check if tag exists
    if(image_tag %in% check_result$imageDetails[[1]]$imageTags){

      return(TRUE)
    }else{
      cat("\n[faasr_msg] image tag ", image_tag, " not exist, please check\n")
      return(FALSE)
      
    }
  }


}


# create lambda role
faasr_register_workflow_aws_lambda_role_create <- function(faasr, cred, lambda_server_info){
  

  # only support one account, even server in different region, only create one role for the account

  # directly provide the role name "faasr-lambda-role" for the user
  aws_lambda_role_name <- "faasr-lambda-role"

  current_lambda_server_info <- lambda_server_info[[1]]
  # aws command to list all roles
  iam_instance <- paws::iam(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=current_lambda_server_info$aws_access_key,
          secret_access_key=current_lambda_server_info$aws_secret_key,
          session_token=""
        )
      ),
      region=current_lambda_server_info$aws_region
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



# Create aws lambda functions
faasr_register_workflow_aws_lambda_function_build <- function(faasr, lambda_function_info, function_image_list, aws_lambda_role_name, cred, lambda_server_info){
  
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
  
  aws_account_id <- lambda_server_info[[1]]$aws_account_id
  #build lambda role arn
  lambda_role_arn <- paste0("arn:aws:iam::",aws_account_id,":role/", aws_lambda_role_name)

  
  #update lambda function configuration if needed
  for (function_name in names(lambda_function_info)){
    
    
    if(lambda_function_info[[function_name]]$action == "update"){
      # get lambda server info for current function
      current_lambda_server_name <- faasr$FunctionList[[function_name]]$FaaSServer
      current_lambda_server_info <- lambda_server_info[[current_lambda_server_name]]

      # build paws lambda instance for the current function
      lambda_instance <- paws::lambda(
        config=list(
          credentials=list(
            creds=list(
              access_key_id=current_lambda_server_info$aws_access_key,
              secret_access_key=current_lambda_server_info$aws_secret_key,
              session_token=""
            )
          ),
          region=current_lambda_server_info$aws_region
        )
      )
      cat("\n[faasr_msg] Will update lambda function configuration", function_name, "\n")
      lambda_instance$update_function_configuration(FunctionName = function_name, Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)
      #print(return_status)
      
    }
  }
  
  # create or update lambda function
  for (function_name in names(lambda_function_info)){
    
    function_image_name <- lambda_function_info[[function_name]]$image
    function_image_url <- function_image_list[[function_image_name]]

    # get lambda server info for current function
    current_lambda_server_name <- faasr$FunctionList[[function_name]]$FaaSServer
    current_lambda_server_info <- lambda_server_info[[current_lambda_server_name]]

    # build paws lambda instance for the current function
    current_lambda_instance <- paws::lambda(
      config=list(
        credentials=list(
          creds=list(
            access_key_id=current_lambda_server_info$aws_access_key,
            secret_access_key=current_lambda_server_info$aws_secret_key,
            session_token=""
          )
        ),
        region=current_lambda_server_info$aws_region
      )
    )

    if(lambda_function_info[[function_name]]$action == "update"){
      cat("\n[faasr_msg] Will update lambda function image", function_name, "\n")
      update_lambda_command <- paste0("aws lambda update-function-code --function-name ",function_name," --image-uri ", function_image_url)
      print(update_lambda_command)
      execute_command_with_retry(function_name, function_image_url, cred, current_lambda_instance)
      
    } else if(lambda_function_info[[function_name]]$action == "create"){
      cat("\n[faasr_msg] Will create lambda function", function_name, "\n")
      # create lambda function
      current_lambda_instance$create_function(FunctionName = function_name, PackageType = "Image", Code = list(ImageUri = function_image_url), Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)  

    }
  }
}


# check if a Lambda function exists
check_lambda_exists <- function(function_name, cred, lambda_server_info) {
  # aws command check if a function exist
  
  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=lambda_server_info$aws_access_key,
          secret_access_key=lambda_server_info$aws_secret_key,
          session_token=""
        )
      ),
      region=lambda_server_info$aws_region
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

execute_command_with_retry <- function(function_name, function_image_url, cred, current_lambda_instance, max_retries = 3, sleep_seconds = 3) {


  print("update function code: execute_command_with_retry")
  

  for (i in 1:max_retries) {
    check_result <- tryCatch({
     result <- current_lambda_instance$update_function_code(FunctionName = function_name, ImageUri = function_image_url)
     
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

  target_server_name <- faasr$FunctionList[[target]]$FaaSServer

  aws_region <- faasr$ComputeServers[[target_server_name]]$Region
  # get access key and secret key from cred
  access_key_name <- paste0(target_server_name, "_ACCESS_KEY")
  secret_key_name <- paste0(target_server_name, "_SECRET_KEY")
  aws_access_key <- cred[[access_key_name]]
  aws_secret_key <- cred[[secret_key_name]]

  # get aws account id
  sts_instance <- paws::sts(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=aws_access_key,
          secret_access_key=aws_secret_key,
          session_token=""
        )
      ),
      region=aws_region
    )
  )
  aws_account_id <- sts_instance$get_caller_identity()$Account
  

  
  faasr_w_cred <- faasr_replace_values(faasr, cred)
  faasr_payload_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox = TRUE)
  
  
  # get the lambda function name
  lambda_function_name <- faasr$FunctionInvoke

  lambda_instance <- paws::lambda(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=aws_access_key,
          secret_access_key=aws_secret_key,
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
          access_key_id=aws_access_key,
          secret_access_key=aws_secret_key,
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
    
    # # Convert the targets list to JSON
    # targets_json <- jsonlite::toJSON(targets, pretty = TRUE, auto_unbox = TRUE)
    
    # # Write faasr_lambda_event_targets.json to file
    # write(targets_json, file = "faasr_lambda_event_targets.json")
    
    # set event target with the faasr_lambda_event_targets.json

    set_lambda_timer_result <- eventbridge_instance$put_targets(Rule = event_rule_name, Targets = targets)
    
    
    #file.remove("faasr_lambda_event_targets.json")
  }
  
}

faasr_workflow_invoke_lambda <- function(faasr, cred, faas_name, actionname){
    aws_region <- faasr$ComputeServers[[faas_name]]$Region
    # get access key and secret key from cred
    access_key_name <- paste0(faas_name, "_ACCESS_KEY")
    secret_key_name <- paste0(faas_name, "_SECRET_KEY")
    aws_access_key <- cred[[access_key_name]]
    aws_secret_key <- cred[[secret_key_name]]
    aws_instance <- paws::lambda(
      config=list(
      credentials=list(
        creds=list(
          access_key_id=aws_access_key,
          secret_access_key=aws_secret_key,
          session_token=""
        )
      ),
      region=aws_region
    )
    )
    # json file with credentials will be created and after invocation, it will be removed.
    faasr_w_cred <- faasr_replace_values(faasr, cred)
    faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)

    cat("waiting for invoke...\n")
    response <- aws_instance$invoke(FunctionName = actionname, Payload = faasr_json)
    if (response$StatusCode == 200) {
    succ_msg <- paste0("Successfully invoked:", actionname, "\n")
    cat(succ_msg)
    
  } else {
    err_msg <- paste0("Error invoking: ",actionname," reason:", response$StatusCode, "\n")
    cat(err_msg)
    
  }

}
                           
