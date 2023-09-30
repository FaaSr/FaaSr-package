.faasr_user <- list()

faasr_register_workflow <- function(){
  svc <- .faasr_get_svc()
  cred <- svc$cred
  faasr <- svc$json
  json_path <- svc$json_path
  cred <- faasr_collect_sys_env(faasr,cred)
  
  faasr_ow <- faasr_register_workflow_ibmcloud_openwhisk(faasr,cred)
  if (is.list(faasr_ow)){
    faasr<-faasr_ow
  }
  faasr_gh <- faasr_register_workflow_github_actions(faasr,cred)
  if (is.list(faasr_gh)){
    faasr<-faasr_gh
  }
  faasr_ld <- faasr_register_workflow_aws_lambda(faasr,cred)
  if (is.list(faasr_ld)){
    faasr<-faasr_ld
  }
  
  json_update <- jsonlite::toJSON(faasr, pretty=TRUE, auto_unbox=TRUE)
  writeLines(json_update, json_path)
  
}
.faasr_user$operations$register_workflow <- faasr_register_workflow

##############################
##############################
##############################
##############################
##############################
##############################
### register_workflow - ow ###
##############################
##############################
##############################
##############################
##############################
##############################

faasr_register_workflow_ibmcloud_openwhisk <- function(faasr, cred) {
  
  # create a server-action set
  action_list <- faasr_register_workflow_ibmcloud_action_lists(faasr)
  
  if (length(action_list)==0){
    return(FALSE)
  }
  
  # check servers and actions, create actions
  for (server in names(action_list)) {
    faasr_register_workflow_ibmcloud_target_group()
    name_id <- faasr_register_workflow_ibmcloud_target_namespace(server, faasr)
    faasr$ComputeServers[[server]]$Namespace <- name_id
    for (act in action_list[[server]]) {
      faasr_register_workflow_ibmcloud_create_action(act, faasr)
    }
  }
  return(faasr)
}


faasr_register_workflow_ibmcloud_action_lists <- function(faasr) {
  # empty list
  action_list <- list()
  # for each function, iteratively collect server names and action names
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is Openwhisk, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      cat("\n\ninvalid server:", server_name," check server type\n\n")
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "OpenWhisk") {
      action_name <- faasr$FunctionList[[fn]]$Actionname
      action_list[[server_name]] <- unique(c(action_list[[server_name]],action_name))
    }
  }
  return(action_list)
}

faasr_register_workflow_ibmcloud_target_group <- function(){
  command <- paste0()
  cat("Target Resource group(Type \"Enter\" to proceed with default value): ")
  check <- readline()
  if (check==""){
    command <- paste0("ibmcloud target -g Default")
  } else{
    command <- paste0("ibmcloud target -g ",check)
  }
  
  response <- system(command, ignore.stdout=TRUE, ignore.stderr=TRUE)
  
  if (response==0){
    cat("\n\nTarget Resource group Success\n\n")
  } else{
    cat("\n\nTarget Resource group Failed, please check resource group\n\n")
    stop()
  }
}

# create a namespace
faasr_register_workflow_ibmcloud_create_namespace <- function(name) {
  command <- paste0("ibmcloud fn namespace create ",name)
  cat("creating a new namespace\n")
  system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
  # get the id of the namespace
  command <- paste0("ibmcloud fn namespace get ",name," --properties")
  response <- system(command, intern=TRUE)
  # parse only ID from the response
  name_id <- sub("^ID:\\s*", "", grep("^ID:", response, value = TRUE))
  return(name_id)
}

# target a namespace
faasr_register_workflow_ibmcloud_target_namespace <- function(server,faasr) {
  namespace<-faasr$ComputeServers[[server]]$Namespace
  command <- paste("ibmcloud fn namespace target",namespace)
  cat("targetting a new namespace\n")
  check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
  # if check == 0, i.e., no errors, return TRUE, retrieve ID in case that a user only doesn't provide ID
  if (check==0) {
    command <- paste0("ibmcloud fn namespace get ",namespace," --properties")
    response <- system(command, intern=TRUE)
    # parse only ID from the response
    name_id <- sub("^ID:\\s*", "", grep("^ID:", response, value = TRUE))
    return(name_id)
    # if check != 0, i.e., errors, ask the user to create a new one
  } else {
    cat("Invalid Namespace\n")
    cat("Create a new Namespace?[y/n]")
    while(TRUE) {
      check <- readline()
      if (check=="y") {
        cat("type Namespace name: ")
        # receive the user's input for the namespace name
        name <- readline()
        # create a new namespace
        namespace <- faasr_register_workflow_ibmcloud_create_namespace(name)
        # save the result to the json file
        faasr$ComputeServers[[server]]$Namespace <- namespace
        cat("successful", "\n")
        faasr_register_workflow_ibmcloud_update_payload(faasr)
        cat("New Namespace name is: ",namespace)
        cat("Please start faasr() again with given payload file: \"faasr_payload_ow.json\"")
        stop()
      } else if(check=="n") {
        cat("stop the function")
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
    # recursively target a namespace
    faasr_register_workflow_ibmcloud_target_namespace(server,faasr)
  }
}

# Not going to be used
# TBD find better way to use this
# login to the ibm cloud by using api keys
faasr_register_workflow_ibmcloud_login_ibm <- function(server,cred) {
  # retrieve api key and try login by using it
  api_key <- cred[[paste0(server,"_API_KEY")]]
  command <- paste("ibmcloud login --apikey",api_key)
  check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE, input="y")
  # if check == 0, i.e., no errors, return TRUE
  if (check==0) {
    cat("\n\n[faasr/msg] Login Success\n\n")
    return(TRUE)
    # if check != 0, i.e., errors, ask the user to create a new one
  } else {
    cat("\n\nInvalid API key\n\n")
    stop()
  }
}


# create an action
faasr_register_workflow_ibmcloud_create_action <- function(actionname, faasr) {
  # actioncontainer can be either default or user-customized
  if (length(faasr$ActionContainers[[actionname]])==0) {
    actioncontainer <- "faasr/openwhisk-tidyverse"
  } else {
    actioncontainer <- faasr$ActionContainers[[actionname]]
  }
  # create a function with maximum timeout and 512MB memory space
  command <- paste("ibmcloud fn action create",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 2048")
  cat("creating a new action\n")
  check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
  # if action already exists, ask the user to update the action
  if (check[1]==153) {
    print("error: action name already exists")
    cat("Do you want to update the action?[y/n]")
    while(TRUE) {
      check <- readline()
      if (check=="y") {
        # update the action
        command <- paste("ibmcloud fn action update",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 2048")
        cat("updating an action\n")
        system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
        cat("successful", "\n")
        break
      } else if(check=="n") {
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
  }
}

##############################
##############################
##############################
##############################
##############################
##############################
### register_workflow - gh ###
##############################
##############################
##############################
##############################
##############################
##############################


faasr_register_workflow_github_actions <- function(faasr, cred) {
  wd <- getwd()
  
  # get repository list
  repo_list <- faasr_register_workflow_github_repo_lists(faasr)
  
  # create repository iteratively
  for (server in names(repo_list)) {
    # login server with the given server's token
    file_name <- paste0(server,"_token.txt")
    writeLines(cred[[paste0(server,"_TOKEN")]], file_name)
    check <- system(paste0("gh auth login --with-token < ",file_name))
    file.remove(file_name)
    if (check == 0){
      cat("\n\n[faasr_msg] login success\n\n")
    }else{
      cat("\n\n[faasr_msg] login failed: please check faasr-Computeservers-server_name-Token\n\n")
      cat("\n\n[faasr_msg] login failed: please also check the Token permission, required: \"read:org\"")
      setwd(wd)
      stop()
    }
    repo <- paste0(faasr$ComputeServers[[server]]$UserName,"/",faasr$ComputeServers[[server]]$ActionRepoName)
    # check the repository
    response <- faasr_register_workflow_github_repo_exists(repo)
    faasr_register_workflow_github_create_env(server,repo,cred)
    faasr_register_workflow_github_set_payload(faasr)
    
    for (actionname in repo_list[[server]]){
      if (length(faasr$ActionContainers[[actionname]]) == 0) {
        faasr_register_workflow_github_create_yml_file("faasr/github-actions-tidyverse",actionname)
      } else {
        container_name <- faasr$ActionContainers[[actionname]]
        faasr_register_workflow_github_create_yml_file(container_name,actionname)
      }
    }
    ref <- faasr$ComputeServers[[server]]$Ref
    faasr_register_workflow_github_gh_setup(response, repo, ref)
    cat("\n\n[faasr_msg] successfully registed server: ", repo,"\n\n")
  }
  
  cat("\n\n[faasr_msg] successfully registed all servers\n\n")
  
  return(faasr)
}


# make a repo list
# like a key-value set, key is a server_name and value is a repository name
faasr_register_workflow_github_repo_lists <- function(faasr) {
  # empty list
  repo_list <- list()
  # for each function, iteratively collect servername and repository name
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is Githubactions, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      cat("\n\n[faasr_msg] invalid server:", server_name," check server type\n\n")
      setwd(wd)
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "GitHubActions") {
      actionname <- paste0(faasr$FunctionList[[fn]]$Actionname)
      repo_list[[server_name]] <- unique(c(repo_list[[server_name]],actionname))
    }
  }
  return(repo_list)
}

# Create an environment
faasr_register_workflow_github_create_env <- function(server_name, repo_name, cred) {
  # split repo into account id and repository name
  repo <- strsplit(repo_name, "/")
  repo_p <- repo[[1]]
  account_id <- repo_p[1]
  repo_name_only <- repo_p[2]
  # create a directory named repository named
  if (!dir.exists(repo_name_only)) {
    dir.create(repo_name_only, recursive=TRUE)  
  } else {
    cat("\n\n[faasr_msg] Directory for the repository already exists\n")
    cat("[faasr_msg] Update?[y/n]")
    while(TRUE) {
      check <- readline()
      if (check == "y") {
        break
      } else if(check == "n") {
        cat("\n\n[faasr_msg] stop the function\n\n")
        setwd(wd)
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
  }
  # set working directory
  setwd(repo_name_only)
  # prepare arguments
  
  cred$PAYLOAD_GITHUB_TOKEN <- cred[[paste0(server_name,"_TOKEN")]]
  secrets_json <- jsonlite::toJSON(cred, auto_unbox=TRUE)
  contents <- paste0("SECRET_PAYLOAD=",secrets_json)
  # create a file ".env"
  writeLines(contents, ".env")
  # create a file ".gitignore"
  writeLines(".env",".gitignore")
  # create a directory ".github/workflows"
  if (!dir.exists(".github/workflows")) {
    dir.create(".github/workflows", recursive=TRUE)
  }
}

# set payload to be uploaded
faasr_register_workflow_github_set_payload <- function(faasr){
  faasr_gh <- faasr
  
  # create a file named "payload.json"
  faasr_gh <- jsonlite::toJSON(faasr_gh, auto_unbox=TRUE)
  faasr_gh_pt <- jsonlite::prettify(faasr_gh)
  write(faasr_gh_pt, "payload.json")
}

# Create a yaml workflow file with the container name
# TBD implement a native workflow pattern
faasr_register_workflow_github_create_yml_file <- function(containername, actionname){
  contents <- paste0("name: Running Actionname- ",actionname,"

on:
  workflow_dispatch:
    inputs:
      ID:
        description: 'InvocationID'
        required: false
      InvokeName:
        description: 'FunctionInvoke'
        required: true
      FaaSrLog:
        description: 'FaaSrLog'
        required: false

jobs:
  run_docker_image:
    runs-on: ubuntu-latest
    container: ",containername,"
    env:
      SECRET_PAYLOAD: ${{ secrets.SECRET_PAYLOAD }}
      PAYLOAD_REPO: ${{ vars.PAYLOAD_REPO }}
      INPUT_ID: ${{ github.event.inputs.ID }}
      INPUT_INVOKENAME: ${{ github.event.inputs.InvokeName }}
      INPUT_FAASRLOG: ${{ github.event.inputs.FaaSrLog }}
    steps:
    - name: run Rscript
      run: |
        cd /action
        Rscript faasr_start_invoke_github-actions.R")
  if (!endsWith(actionname,".yml")){
    actionname <- paste0(actionname,".yml")
  }
  path <- paste0(".github/workflows/",actionname)
  writeLines(contents, path)
}

# setup a git repository
faasr_register_workflow_github_gh_setup <- function(check, repo, ref) {
  # split repo into account id and repository name
  repo_a <- strsplit(repo, "/")
  repo_p <- repo_a[[1]]
  account_id <- repo_p[1]
  repo_name <- repo_p[2]
  # check whether git remote repository already exists
  if (check==FALSE) {
    # if not, build a new repository
    # Ask user for the repository to be private or public
    cat("[private/public]")
    while(TRUE) {
      check <- readline()
      if (check == "private") {
        auth <- "private"
        break
      } else if(check == "public") {
        auth <- "public"
        break
      } else {
        cat("Enter \"private\" or \"public\": \n")
      }
    }
    if (dir.exists(".git")){
      unlink(".git", recursive=TRUE)
    }
    # create a local git repository
    system("git init")
    msg <- paste0("git branch -m ", ref)
    system(msg)
    system("git add .")
    system("git commit -m \'build repo\'")
    cat("\n\n[faasr_msg] Create the repository\n")
    # create a remote git repository
    check <- system(paste0("gh repo create ", repo_name, " --",auth," --push --source=."))
    if (check==0){
      cat("\n\n[faasr_msg] Successfully create the repo\n")
    } else{
      cat("\n\n[faasr_msg] Error: Failed to create the repo\n")
      setwd(wd)
      stop()
    }
  } else if (check == TRUE) {
    # if the repository already exists, ask user to update it or not
    cat("\n\n[faasr_msg] Repository already exists\n")
    cat("Update the repository?[y/n]")
    while(TRUE) {
      check1 <- readline()
      if (check1=="y") {
        break
      } else if(check1 == "n") {
        cat("\n\n[faasr_msg]stop the function\n")
        setwd(wd)
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
    # create a local git repository
    system("git init")
    msg <- paste0("git checkout -B ", ref)
    system(msg)
    system("git add .")
    system("git commit -m \'update repo\'")
    # push it to the remote git repository
    command <- paste0("git push -f http://github.com/", repo, " ", ref)
    check2 <- system(command)
    if (check2==0){
      cat("\n\n[faasr_msg] Successfully update the repo\n")
    } else{
      cat("\n\n[faasr_msg] Error: Failed to update the repo\n")
    }
  }
  # set secrets and variables
  system(paste0("gh secret set -f .env --repo ", repo))
  system(paste0('gh variable set PAYLOAD_REPO --body ', repo,'/payload.json',' --repo ',repo))
  # return to the default directory to make another one
  setwd("..")
}

# check the repository
faasr_register_workflow_github_repo_exists <- function(repo) {
  system_command <- paste0("gh repo view ", repo, " > /dev/null 2>&1")
  system_command <- paste0("gh repo view ", repo)
  exit_code <- system(system_command)
  # if it exists, it returns TRUE, if not, it returns FALSE
  return(exit_code == 0)
}


##############################
##############################
##############################
##############################
##############################
##############################
### register_workflow - ld ###
##############################
##############################
##############################
##############################
##############################
##############################

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


##############################
##############################
##############################
##############################
##############################
##############################
### setup conf/cred native ###
##############################
##############################
##############################
##############################
##############################
##############################

faasr_collect_sys_env <- function(faasr, cred){
  for (faas_cred in names(faasr$ComputeServers)){
    if (faasr$ComputeServers[[faas_cred]]$FaaSType=="GitHubActions"){
      cred_name <- faasr$ComputeServers[[faas_cred]]$Token
      if (is.null(cred_name)){
        cred_name <- paste0(faas_cred, "_TOKEN")
      }
      if (is.null(cred[[cred_name]])){
        real_cred <- Sys.getenv(cred_name)
        if (real_cred == ""){
          cat("\n\n[faasr_msg] ",cred_name," requires values\n\n")
        } else{
          cred[[cred_name]] <- real_cred
        }
      }
      
    } else if (faasr$ComputeServers[[faas_cred]]$FaaSType=="OpenWhisk"){
      cred_name <- faasr$ComputeServers[[faas_cred]]$API.key
      if (is.null(cred_name)){
        cred_name <- paste0(faas_cred, "_API_KEY")
      }
      if (is.null(cred[[cred_name]])){
        real_cred <- Sys.getenv(cred_name)
        if (real_cred == ""){
          cat("\n\n[faasr_msg] ",cred_name," requires values\n\n")
        } else{
          cred[[cred_name]] <- real_cred
        }
      }
      
    } else if (faasr$ComputeServers[[faas_cred]]$FaaSType=="Lambda"){
      cred_name_ac <- faasr$ComputeServers[[faas_cred]]$AccessKey
      if (is.null(cred_name_ac)){
        cred_name_ac <- paste0(faas_cred, "_ACCESS_KEY")
      }
      if (is.null(cred[[cred_name_ac]])){
        real_cred <- Sys.getenv(cred_name_ac)
        if (real_cred == ""){
          cat("\n\n[faasr_msg] ",cred_name_ac," requires values\n\n")
        } else{
          cred[[cred_name_ac]] <- real_cred
        }
      }
      
      cred_name_sc <- faasr$ComputeServers[[faas_cred]]$SecretKey
      if (is.null(cred_name_sc)){
        cred_name_sc <- paste0(faas_cred, "_SECRET_KEY")
      }
      if (is.null(cred[[cred_name_sc]])){
        real_cred <- Sys.getenv(cred_name_sc)
        if (real_cred == ""){
          cat("\n\n[faasr_msg] ",cred_name_sc," requires values\n\n")
        } else{
          cred[[cred_name_sc]] <- real_cred
        }
      }
    }
  }
  
  for (data_cred in names(faasr$DataStores)){
    cred_name_ac <- faasr$DataStores[[data_cred]]$AccessKey
    if (is.null(cred_name_ac)){
      cred_name_ac <- paste0(faas_cred, "_ACCESS_KEY")
    }
    if (is.null(cred[[cred_name_ac]])){
      real_cred <- Sys.getenv(cred_name_ac)
      if (real_cred == ""){
        cat("\n\n[faasr_msg] ",cred_name_ac," requires values\n\n")
      } else{
        cred[[cred_name_ac]] <- real_cred
      }
    }
    cred_name_sc <- faasr$DataStores[[data_cred]]$SecretKey
    if (is.null(cred_name_sc)){
      cred_name_sc <- paste0(faas_cred, "_SECRET_KEY")
    }
    if (is.null(cred[[cred_name_sc]])){
      real_cred <- Sys.getenv(cred_name_sc)
      if (real_cred == ""){
        cat("\n\n[faasr_msg] ",cred_name_sc," requires values\n\n")
      } else{
        cred[[cred_name_sc]] <- real_cred
      }
    }
  }
  return(cred)
}


.faasr_get_svc <- function(){
  calling_env <- parent.frame(2)
  call <- sys.call(-1)[[1]]
  if (is.function(call)) {
    call <- sys.call(-2)[[2]]
  }
  object <- eval(call[[2]], envir = calling_env)
  return(object)
}


faasr <- function(json_path, env_path=NULL){
  wd <- getwd()
  svc <- .faasr_user$operations
  svc$cred <- list()
  svc$json <- jsonlite::fromJSON(json_path)
  
  for (faas_js in names(svc$json$ComputeServers)){
    switch (svc$json$ComputeServers[[faas_js]]$FaaSType,
            "GitHubActions"={
              if (!is.null(svc$json$ComputeServers[[faas_js]]$Token)){
                if (svc$json$ComputeServers[[faas_js]]$Token != paste0(faas_js,"_TOKEN")){
                  svc$cred[[paste0(faas_js,"_TOKEN")]] <- svc$json$ComputeServers[[faas_js]]$Token
                }
              }
              svc$json$ComputeServers[[faas_js]]$Token <- paste0(faas_js,"_TOKEN")
            },
            "Lambda"={
              if (!is.null(svc$json$ComputeServers[[faas_js]]$AccessKey)){
                if (svc$json$ComputeServers[[faas_js]]$AccessKey != paste0(faas_js,"_ACCESS_KEY")){
                  svc$cred[[paste0(faas_js,"_ACCESS_KEY")]] <- svc$json$ComputeServers[[faas_js]]$AccessKey
                }
              }
              svc$json$ComputeServers[[faas_js]]$AccessKey <- paste0(faas_js,"_ACCESS_KEY")
              
              if (!is.null(svc$json$ComputeServers[[faas_js]]$SecretKey)){
                if (svc$json$ComputeServers[[faas_js]]$SecretKey != paste0(faas_js,"_SECRET_KEY")){
                  svc$cred[[paste0(faas_js,"_SECRET_KEY")]] <- svc$json$ComputeServers[[faas_js]]$SecretKey
                }
              }
              svc$json$ComputeServers[[faas_js]]$SecretKey <- paste0(faas_js,"_SECRET_KEY")
            },
            "OpenWhisk"={
              if (!is.null(svc$json$ComputeServers[[faas_js]]$API.key)){
                if (svc$json$ComputeServers[[faas_js]]$API.key != paste0(faas_js,"_API_KEY")){
                  svc$cred[[paste0(faas_js,"_API_KEY")]] <- svc$json$ComputeServers[[faas_js]]$API.key
                }
              }
              svc$json$ComputeServers[[faas_js]]$API.key <- paste0(faas_js,"_API_KEY")
            }
    )
  }
  
  for (data_js in names(svc$json$DataStores)){
    if (!is.null(svc$json$DataStores[[data_js]]$AccessKey)){
      if (svc$json$DataStores[[data_js]]$AccessKey != paste0(data_js,"_ACCESS_KEY")){
        svc$cred[[paste0(data_js,"_ACCESS_KEY")]] <- svc$json$DataStores[[data_js]]$AccessKey
      }
    }
    svc$json$DataStores[[data_js]]$AccessKey <- paste0(data_js,"_ACCESS_KEY")
    
    if (!is.null(svc$json$DataStores[[data_js]]$AccessKey)){
      if (svc$json$DataStores[[data_js]]$AccessKey != paste0(data_js,"_ACCESS_KEY")){
        svc$cred[[paste0(data_js,"_ACCESS_KEY")]] <- svc$json$DataStores[[data_js]]$AccessKey
      }
    }
    svc$json$DataStores[[data_js]]$SecretKey <- paste0(data_js,"_SECRET_KEY")
  }
  
  
  if (!is.null(env_path)){
    envs <- readLines(env_path, warn=FALSE)
    
    for (env in envs){
      env_parts <- strsplit(env, "=")
      if (length(env_parts[[1]]) == 2) {
        env_key <- trimws(gsub("[\"]", "", env_parts[[1]][1]))
        env_value <- trimws(gsub("[\"\",]", "", env_parts[[1]][2]))
        svc$cred[[env_key]] <- env_value
      }
    }
  }
  
  if (!dir.exists(".faasr_json")){
    dir.create(".faasr_json")
  }
  rd_nb <- sample(1:1000000, size=1)
  json_path <- paste0(wd,"/.faasr_json/.faasr_json_",rd_nb)
  json_update <- jsonlite::toJSON(svc$json, pretty=TRUE, auto_unbox=TRUE)
  writeLines(json_update, json_path)
  svc$json_path <- json_path
  
  return(svc)
}


##############################
##############################
##############################
##############################
##############################
##############################
####### invoke_workflow ######
##############################
##############################
##############################
##############################
##############################
##############################


faasr_replace_values <- function(faasr, cred){
  for (name in names(faasr)) {
    if (name == "FunctionList") {
      next
    }
    # If the value is a list, call this function recursively
    if (is.list(faasr[[name]])) {
      faasr[[name]] <- faasr_replace_values(faasr[[name]], cred)
    } else {
      # If the value exists in the secrets, replace it
      if (faasr[[name]] %in% names(cred)) {
        faasr[[name]] <- cred[[faasr[[name]]]]
      }
    }
  }
  return(faasr)
}


faasr_invoke_workflow <- function(FunctionInvoke=NULL){
  svc <- .faasr_get_svc()
  cred <- svc$cred
  json_path <- svc$json_path
  faasr <- jsonlite::fromJSON(json_path)
  cred <- faasr_collect_sys_env(faasr,cred)
  
  if (!is.null(FunctionInvoke)){
    functioninvoke <- FunctionInvoke
  } else{
    functioninvoke <- faasr$FunctionInvoke
  }
  
  faas_name <- faasr$FunctionList[[functioninvoke]]$FaaSServer
  faas_type <- faasr$ComputeServers[[faas_name]]$FaaSType
  actionname <- faasr$FunctionList[[functioninvoke]]$Actionname
  
  switch(faas_type,
         "GitHubActions"={
           gh_ref <- faasr$ComputeServers[[faas_name]]$Ref
           repo <- paste0(faasr$ComputeServers[[faas_name]]$UserName,"/",faasr$ComputeServers[[faas_name]]$ActionRepoName)
           command <- paste0("gh workflow run --repo ",repo," --ref ",gh_ref," ",actionname," -f InvokeName=",functioninvoke)
           check <- system(command)
         },
         "Lambda"={
           faasr_w_cred <- faasr_replace_values(faasr, cred)
           faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
           rd_nb <- sample(100000, size=1)
           writeLines(faasr_json, paste0("payload_ld_",rd_nb,".json"))
           command <- paste0("aws lambda invoke --function-name ",functioninvoke," --cli-binary-format raw-in-base64-out --invocation-type RequestResponse --payload file://",paste0("payload_ld_",rd_nb,".json")," lambda_outputfile.txt")
           check <- system(command)
         },
         "OpenWhisk"={
           faasr_w_cred <- faasr_replace_values(faasr, cred)
           faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
           rd_nb <- sample(100000, size=1)
           writeLines(faasr_json, paste0("payload_ow_",rd_nb,".json"))
           command <- paste0("ibmcloud fn action invoke ",actionname," --blocking --param-file ",json_path)
           check <- system(command)
           file.remove(paste0("payload_ow_",rd_nb,".json"))
         })
}
.faasr_user$operations$invoke_workflow <- faasr_invoke_workflow


