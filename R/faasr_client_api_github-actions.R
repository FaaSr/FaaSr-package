#' @title Client tool for registering github-actions.
#' @description faasr_register_workflow_github_actions: 
#' starts to register functions in the JSON file for GithubActions
#' @description faasr_register_workflow_github_repo_lists: 
#' creates a list of Server and Action name pairs               
#' @description faasr_register_workflow_github_create_env: 
#' creates a "env" file for the credentials for github actions
#' @description faasr_register_workflow_github_create_yml_file:
#' creates a "yml" file, name with the given actionname and a folder ".github/workflows/"             
#' @description faasr_register_workflow_github_set_payload: 
#' creates a "payload" file for json            
#' @description faasr_register_workflow_github_gh_setup: 
#' create a local repo, push the local to remote, and setup payload and credentials             
#' @description faasr_register_workflow_github_repo_exists:
#' check the given repo in the JSON exists              
#'
#' @param faasr_register_workflow_github_actions: "faasr" for list of the json, "cred" for the list of credentials
#' @param faasr_register_workflow_github_repo_lists: "faasr" for list of the json
#' @param faasr_register_workflow_github_create_env: "server_name" for the string of server name, 
#' "repo_name" for the string of repo name, "cred" for the list of credentials
#' @param faasr_register_workflow_github_create_yml_file: "faasr" for list of the json
#' @param faasr_register_workflow_github_set_payload: "faasr" for list of the json
#' @param faasr_register_workflow_github_gh_setup: "check" for the boolean, "repo" for the string, "ref" for the string
#' @param faasr_register_workflow_github_repo_exists: "repo" for the string
#' 
#' @return faasr_register_workflow_github_actions: "faasr" for list of the json
#' @return faasr_register_workflow_github_repo_lists: "repo_list" for the list of servername:actionname pairs
#' @return faasr_register_workflow_github_repo_exists: "exit_code" for the int 


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
    faasr_register_workflow_github_readme()
    
    for (actionname in repo_list[[server]]){
      if (length(faasr$ActionContainers[[actionname]]) == 0 || faasr$ActionContainers[[actionname]] == "") {
        faasr_register_workflow_github_create_yml_file("faasr/github-actions-tidyverse",actionname)
      } else {
        container_name <- faasr$ActionContainers[[actionname]]
        faasr_register_workflow_github_create_yml_file(container_name,actionname)
      }
    }
    ref <- faasr$ComputeServers[[server]]$Branch
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
      actionname <- fn
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
  contents <- paste0("SECRET_PAYLOAD=",secrets_json,",","REPO_TOKEN",cred[[paste0(server_name,"_TOKEN")]])
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
  contents <- paste0("name: Running Action- ",actionname,"

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
      GITHUB_PAT: ${{ secrets.REPO_TOKEN }}
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
    cat("[faasr_msg] Update the repository?[y/n]")
    while(TRUE) {
      check1 <- readline()
      if (check1=="y") {
        break
      } else if(check1 == "n") {
        cat("\n\n[faasr_msg] Stop the function\n")
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

# create README.md file for repository description
faasr_register_workflow_github_readme <- function(){
  contents <- paste0("# This is an automatically generated FaaSr repository 

This repository has been created automatically by the FaaSr register_workflow() function.</br>
It stores the workflow .yml files for your FaaSr workflow, as well as the JSON configuration and secrets.</br>
It is safe to delete this repository if you no longer need this workflow. It can be re-created by running register_workflow()</br>")
  path <- "README.md"
  writeLines(contents, path)
}

# set workflow timer for github
faasr_set_workflow_timer_gh <- function(faasr, target, cron, unset=FALSE){

  # bring variables
  folder <- faasr$FaaSrLog
  id <- faasr$InvocationID

  # Note: github only accepts less frequnt than a job per 5min
  cat("[faasr_msg] Be cautious that mininum cron timer for github actions is 5minutes (*/5 * * * *), yours:", cron)
  # set yaml file name
  if (!endsWith(target,".yml")){
    target_yml <- paste0(target,".yml")
  } else {
    target_yml <- target
  }

  # bring the workflow file path
  # Note that the github local repository should reside in the current directory
  workflow <- paste0(faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName,"/.github/workflows/",target_yml)
  if (!file.exists(workflow)){
    cat("\n\n[faasr_msg]Check that current working directory is correct and/or local repository exists\n\n")
    cat("\n\n[faasr_msg]Error: No workflow file found\n\n")
    stop()
  }
  # Contents of workflow yaml file differs depending on the set/unset parameter
  # Content_2 indicates the timer
  contents_1 <- paste0("name: Running Action- ",target,"

on:")
  contents_2 <- paste0("
  schedule:
    - cron: \"",cron,"\"")
  contents_3 <- paste0("
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
    container: ",faasr$ActionContainers[[target]],"
    env:
      SECRET_PAYLOAD: ${{ secrets.SECRET_PAYLOAD }}
      PAYLOAD_REPO: ${{ vars.PAYLOAD_REPO }}
      GITHUB_PAT: ${{ secrets.REPO_TOKEN }}
      INPUT_ID: ${{ github.event.inputs.ID || \'",id,"\'  }}
      INPUT_INVOKENAME: ${{ github.event.inputs.InvokeName || \'",target,"\' }}
      INPUT_FAASRLOG: ${{ github.event.inputs.FaaSrLog || \'",folder,"\'  }}
    steps:
    - name: run Rscript
      run: |
        cd /action
        Rscript faasr_start_invoke_github-actions.R")
  
  if (unset==TRUE){
    contents_2 <- NULL
  }
  contents <- paste0(contents_1, contents_2, contents_3)
  # create a workflow yaml file
  writeLines(contents, workflow)
  wd <- getwd()
  # get into the local directory/ set variables
  setwd(faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName)
  user_name <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$UserName
  repo_name <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName
  repo <- paste0(user_name,"/",repo_name)
  ref <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$Branch
  # start git init/checkout/push
  system("git init")
  msg <- paste0("git checkout -B ", ref)
  system(msg)
  system("git add .")
  system("git commit -m \'update repo\'")
  command <- paste0("git push -f http://github.com/", repo, " ", ref)
  check2 <- system(command)
  # check the result
  if (check2==0){
    cat("\n\n[faasr_msg] Successfully update the repo with cron timer\n")
  } else{
    cat("\n\n[faasr_msg] Error: Failed to update the repo with cron timer\n")
  }
  # get out of the local repository
  setwd(wd)
}



