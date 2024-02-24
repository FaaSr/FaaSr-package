#' @name faasr_register_workflow_aws_lambda
#' @title faasr_register_workflow_aws_lambda
#' @description 
#' starts to register functions in the JSON file for AWS-Lambda
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param memory an integer for the max size of memory
#' @param timeout an integer for the max length of timeout
#' @import cli
#' @keywords internal

faasr_register_workflow_aws_lambda <- function(faasr, cred, memory=1024, timeout=600){

  # check function information
  lambda_function_info <- faasr_register_workflow_lambda_function_lists(faasr)
  
  if (length(lambda_function_info)==0){
    return("")
  }
    
  cli::cli_h1(paste0("Registering workflow for lambda"))
  cli::cli_progress_bar(
    format = paste0(
      "FaaSr {pb_spin} Registering workflow lambda ",
      "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
    ),
    format_done = paste0(
      "{col_yellow(symbol$checkbox_on)} Successfully registered actions for lambda server ",
      "in {pb_elapsed}."
    ),
    total = 5
  )

  lambda_server_info <- faasr_register_workflow_lambda_server_info(faasr, cred)
  cli_progress_update()

  # get aws lambda function list
  lambda_function_info <- faasr_register_workflow_lambda_function_info(faasr, cred, lambda_server_info, lambda_function_info)
  cli_progress_update()

  # get aws lambda function image list
  function_image_list <- faasr_register_workflow_lambda_function_image(faasr, lambda_server_info)
  cli_progress_update()

  #create aws lambda function role
  aws_lambda_role_name <- faasr_register_workflow_aws_lambda_role_create(faasr, cred, lambda_server_info)
  cli_progress_update()

  # create aws lambda functions
  faasr_register_workflow_aws_lambda_function_build(faasr, lambda_function_info, function_image_list, aws_lambda_role_name, cred, lambda_server_info, memory, timeout)
  cli_progress_update()

  cli_text(col_cyan("{symbol$menu} {.strong Successfully registered all lambda actions}"))
}


#' @title faasr_register_workflow_lambda_function_lists
#' @description 
#' Get aws lambda function list
#' @param faasr a list form of the JSON file
#' @return lambda_function_info a list form of lambda function information: name, actions
#' @import cli
#' @keywords internal

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
  return(lambda_function_info)
}


#' @title faasr_register_workflow_lambda_function_info
#' @description 
#' Get aws lambda function information
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param lambda_server_info a list form of Lambda server information: id, keys, region
#' @param lambda_function_info a list form of lambda function information: name, actions
#' @return lambda_function_info a list form of lambda function information: name, actions, create/update
#' @import cli
#' @keywords internal

# Get aws lambda function info
faasr_register_workflow_lambda_function_info <- function(faasr,cred, lambda_server_info, lambda_function_info){
  
  function_list <- faasr$FunctionList
  compute_servers <- faasr$ComputeServers
  
  for(action_name in names(lambda_function_info)){
    server_name <- function_list[[action_name]]$FaaSServer
    current_lambda_server_info <- lambda_server_info[[server_name]]

    if(check_lambda_exists(action_name, cred, current_lambda_server_info)){
      cli_alert_info(paste0("lambda function - {.strong ", action_name, "} already exists."))
      cli_alert_info("Do you want to update?[y/n]")
      while(TRUE) {
        check <- readline()
        if(check == "y" || check == ""){
          lambda_function_info[[action_name]]$action <- "update"
          break
        } else if(check == 'n'){
          cli_alert_danger("stop the script")
          stop()
        }else {
          cli_alert_warning("Enter \"y\" or \"n\": ")
        }
      }
    } else {
      lambda_function_info[[action_name]]$action <- "create"
    }
  }
  cli_alert_success("Get the lambda function information")

  return (lambda_function_info)
}


#' @title faasr_register_workflow_lambda_server_info
#' @description 
#' Get aws lambda server information
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @return lambda_server_info a list form of Lambda server information: id, keys, region
#' @import cli
#' @importFrom "paws.security.identity" "sts"
#' @keywords internal

faasr_register_workflow_lambda_server_info <- function(faasr, cred){
  
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
        sts_instance <- paws.security.identity::sts(
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
  return(lambda_server_info)
}


#' @title faasr_register_workflow_lambda_function_image
#' @description 
#' Get aws lambda function image list
#' @param faasr a list form of the JSON file
#' @param lambda_server_info a list form of Lambda server information: id, keys, region
#' @return function_image_list a list form of lambda function information: name, images
#' @import cli
#' @keywords internal

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
            cli_alert_danger("stop the script")
            stop()
          }
          image_path <- user_image_url
        }
        function_image_list[[action_name]] <- image_path
      }
    }
  }
  cli_alert_success("Get the ECR image information")
  return (function_image_list)
}


#' @title check_user_image_exist
#' @description 
#' check if user provided image exists, if not, return false then stop processing
#' @param faasr a list form of the JSON file
#' @param action_name a string for the target action name 
#' @param server_name a string for the target server
#' @param user_image_url a string for FaaSr container image uri
#' @param current_lambda_server_info a list form of current Lambda server information: id, keys, region
#' @return logical value TRUE/FALSE for the existence of user images.
#' @import cli
#' @importFrom "paws.compute" "ecr"
#' @keywords internal

# check if user provided image exists, if not, return false then stop processing
check_user_image_exist <- function(faasr, action_name, server_name, user_image_url, current_lambda_server_info){
  # Split by '/' and then by ':'
  splited_img <- unlist(strsplit(user_image_url, "/"))
  splited_image_part <- splited_img[2]
  if (startsWith(splited_img[1], basic_ld_image_account)){
    aws_account_id <- basic_ld_image_account_id
  } else {
    aws_account_id <- current_lambda_server_info$aws_account_id
  }
  repo_tag_part <- unlist(strsplit(splited_image_part, ":"))
  repo_name <- repo_tag_part[1]
  image_tag <- repo_tag_part[2]

  ecr_instance <- paws.compute::ecr(
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
    result <- ecr_instance$batch_get_image(registryId = aws_account_id, repositoryName = repo_name, imageIds=list(list(imageTag=image_tag)))
  }, error = function(e) {
    # Check if the error is a RepositoryNotFoundException
    if(grepl("HTTP 400", e$message)) {
      cli_alert_danger(paste0("Check repository error: ", e$message))
      stop()  
    }
  })

  # check if the repo exists
  if (length(check_result$failure) == 0) {
    return(TRUE)
  }else{
    err_msg <- paste0("image tag ", image_tag, " not exist, please check")
    cli_alert_warning(err_msg)
    return(FALSE)
  }
}


#' @title faasr_register_workflow_aws_lambda_role_create
#' @description 
#' create the aws-lambda role named "faasr-lambda-role"
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param lambda_server_info a list form of Lambda server information: id, keys, region
#' @return lambda-role-name a string for the lambda role name
#' @import cli
#' @importFrom "paws.security.identity" "iam"
#' @keywords internal

# create lambda role
faasr_register_workflow_aws_lambda_role_create <- function(faasr, cred, lambda_server_info){
  

  # only support one account, even server in different region, only create one role for the account

  # directly provide the role name "faasr-lambda-role" for the user
  aws_lambda_role_name <- "faasr-lambda-role"

  current_lambda_server_info <- lambda_server_info[[1]]
  # aws command to list all roles
  iam_instance <- paws.security.identity::iam(
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
    
    # attach policy to role
    iam_instance$attach_role_policy(RoleName = aws_lambda_role_name, PolicyArn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole")
    
    cli_alert_success(paste("Created role:", aws_lambda_role_name))
    
  } else {
    cli_alert_success(paste("Role", aws_lambda_role_name, "already exists."))
  }
  
  return (aws_lambda_role_name)
}


#' @title faasr_register_workflow_aws_lambda_function_build
#' @description 
#' Create aws lambda functions
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param lambda_function_info a list form of lambda function information: name, actions
#' @param function_image_list a list form of lambda function information: name, images
#' @param aws_lambda_role_name a string for the lambda role name
#' @param lambda_server_info a list form of Lambda server information: id, keys, region
#' @param memory an integer for the max size of memory
#' @param timeout an integer for the max length of timeout
#' @importFrom "paws.compute" "lambda"
#' @import cli
#' @keywords internal

# Create aws lambda functions
faasr_register_workflow_aws_lambda_function_build <- function(faasr, lambda_function_info, function_image_list, aws_lambda_role_name, cred, lambda_server_info, memory=1024, timeout=600){
  
  # set configuration for new lambda function
  # ask user to specify the function timeout and memory size
  has_create <- any(sapply(lambda_function_info, function(x) x$action == "create"))
  #if(has_create){
  
  aws_lambda_timeout <- as.numeric(timeout)

  if(aws_lambda_timeout >= 900 && aws_lambda_timeout <= 60){
    cli_alert_danger("Invalid timeout Please provide a numeric value between 60 and 900")
    stop()
  }  
  
  aws_lambda_memory <- as.numeric(memory)

  # Check if the input is numeric and between 256 and 10240
  if(aws_lambda_memory >= 10240 && aws_lambda_memory <= 256){
    cli_alert_danger("Invalid memory size. Please provide a numeric value between 256 and 10240")
    stop()
  }
  
  aws_account_id <- lambda_server_info[[1]]$aws_account_id
  #build lambda role arn
  lambda_role_arn <- paste0("arn:aws:iam::",aws_account_id,":role/", aws_lambda_role_name)

  
  # create or update lambda function
  for (function_name in names(lambda_function_info)){
    
    function_image_name <- lambda_function_info[[function_name]]$image
    function_image_url <- function_image_list[[function_image_name]]

    # get lambda server info for current function
    current_lambda_server_name <- faasr$FunctionList[[function_name]]$FaaSServer
    current_lambda_server_info <- lambda_server_info[[current_lambda_server_name]]

    # build paws lambda instance for the current function
    current_lambda_instance <- paws.compute::lambda(
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
      current_lambda_instance$update_function_configuration(FunctionName = function_name, Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)
      execute_command_with_retry(function_name, function_image_url, cred, current_lambda_instance)
      cli_alert_success(paste0("Successfully Update the function: ", function_name))
      
    } else if(lambda_function_info[[function_name]]$action == "create"){
      current_lambda_instance$create_function(FunctionName = function_name, PackageType = "Image", Code = list(ImageUri = function_image_url), Role = lambda_role_arn, Timeout = aws_lambda_timeout, MemorySize = aws_lambda_memory)  
      cli_alert_success(paste0("Successfully Create the function: ", function_name))
    }
  }
}


#' @title check_lambda_exists
#' @description 
#' check if a Lambda function exists
#' @param function_name a string for the function name
#' @param cred a list form of the credentials
#' @param lambda_server_info a list form of Lambda server information: id, keys, region
#' @import cli
#' @importFrom "paws.compute" "lambda"
#' @keywords internal

# check if a Lambda function exists
check_lambda_exists <- function(function_name, cred, lambda_server_info) {
  # aws command check if a function exist
  
  lambda_instance <- paws.compute::lambda(
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


#' @title execute_command_with_retry
#' @description 
#' check if aws command run successfully, and retry
#' @param function_name a string for the function name
#' @param function_image_url a string for FaaSr container image uri
#' @param cred a list form of the credentials
#' @param current_lambda_instance a list form of current Lambda server information: id, keys, region
#' @param max_retries a integer for the number of maximum tries
#' @param sleep_seconds a integer for the time for sleep between retries
#' @return a logical value
#' @import cli
#' @keywords internal

# check if aws command run successfully, and retry
execute_command_with_retry <- function(function_name, function_image_url, cred, current_lambda_instance, max_retries = 3, sleep_seconds = 5) {

  for (i in 1:max_retries) {
    # Pause before retrying
    Sys.sleep(sleep_seconds)

    check_result <- tryCatch({
     result <- current_lambda_instance$update_function_code(FunctionName = function_name, ImageUri = function_image_url)
     
      }, error = function(e) {
        # Check if the error is a RepositoryNotFoundException
        if(grepl("HTTP 409", e$message)) {
          return(409)
        } else {
          cli_alert_danger(paste0("Update functions error: ", e))
          stop()
        }
      })
    # check the status code to see if command failed
    if (length(check_result)== 1 && check_result == 409) {
    } else {
      return(TRUE)
    }
    
  }
  cli_alert_danger("Max retries reached. Exiting.")
  stop()
}


#' @title faasr_set_workflow_timer_ld
#' @description 
#' # set/unset workflow cron timer for lambda
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param target a string for the target action
#' @param cron a string for cron data e.g., */5 * * * *
#' @param unset a logical value; set timer(FALSE) or unset timer(TRUE)
#' @import cli
#' @importFrom "paws.compute" "lambda"
#' @importFrom "paws.security.identity" "sts"
#' @importFrom "paws.application.integration" "eventbridge"
#' @keywords internal

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
  sts_instance <- paws.security.identity::sts(
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

  lambda_instance <- paws.compute::lambda(
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


  eventbridge_instance <- paws.application.integration::eventbridge(
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
    cli_alert_info(paste0("event schedule: cron(",  event_cron_rule, ")\n"))
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
      cli_alert_info("permission statement already exist")
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

    set_lambda_timer_result <- eventbridge_instance$put_targets(Rule = event_rule_name, Targets = targets)
    cli_alert_success("Succesfully set the cron timer")
  }
  
}


#' @title faasr_workflow_invoke_lambda
#' @description 
#' Invoke a workflow for the lambda
#' this function is invoked by faasr_workflow_invoke
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param faas_name a string for the target server
#' @param actionname a string for the target action name
#' @import cli
#' @importFrom "paws.compute" "lambda"
#' @keywords internal

faasr_workflow_invoke_lambda <- function(faasr, cred, faas_name, actionname){
    aws_region <- faasr$ComputeServers[[faas_name]]$Region
    # get access key and secret key from cred
    access_key_name <- paste0(faas_name, "_ACCESS_KEY")
    secret_key_name <- paste0(faas_name, "_SECRET_KEY")
    aws_access_key <- cred[[access_key_name]]
    aws_secret_key <- cred[[secret_key_name]]
    aws_instance <- paws.compute::lambda(
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

    cli_alert_info("waiting for invoke...")
    response <- aws_instance$invoke(FunctionName = actionname, Payload = faasr_json)
    if (response$StatusCode == 200) {
    succ_msg <- paste0("Successfully invoked:", actionname, "\n")
    cli_alert_success(succ_msg)
    
  } else {
    err_msg <- paste0("Error invoking: ",actionname," reason:", response$StatusCode, "\n")
    cli_alert_danger(err_msg)
    
  }
}
                           
