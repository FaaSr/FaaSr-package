workflow_basic_path <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/workflow_template.yml"
workflow_timer_path <- "https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/workflow_with_cron_template.yml"
#TBD. workflow_runner_path <- ""
#TBD. workflow_runner_w_timer_path <- ""

#' @name faasr_register_workflow_github_actions
#' @title faasr_register_workflow_github_actions
#' @description 
#' register the workflow for github actions.
#' parse faasr to get the repository list and actions.
#' create a local/remote repository for the FaaSr actions.
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param cron a string for cron data e.g., */5 * * * *
#' @param runner a logical value; enable runner
#' @import httr
#' @import cli
#' @export

faasr_register_workflow_github_actions <- function(faasr, cred, cron=NULL, runner=FALSE) {

  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")

  # get a repo list
  repo_list <- faasr_register_workflow_github_repo_lists(faasr)

  if (length(repo_list)==0){
    return("")
  }
  for (server in names(repo_list)) {
    # get env
    token <- cred[[paste0(server,"_TOKEN")]]
    ref <- faasr$ComputeServers[[server]]$Branch
    repo <- paste0(faasr$ComputeServers[[server]]$UserName,"/",faasr$ComputeServers[[server]]$ActionRepoName)
    
    cli::cli_h1(paste0("Registering workflow github actions for repo: ", server))
    cli::cli_progress_bar(
      format = paste0(
        "FaaSr {pb_spin} Registering workflow github actions ",
        "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
      ),
      format_done = paste0(
        "{col_yellow(symbol$checkbox_on)} Successfully registered repo {repo} ",
        "in {pb_elapsed}."
      ),
      total = 4
    )

    # check the repository
    response <- faasr_register_workflow_github_repo_exists(token,repo)
    # check user's request
    private <- faasr_register_workflow_github_repo_question(response)
    # create directories
    faasr_register_workflow_github_create_dir(server,repo)
    cli_alert_success("Create github local directories")
    # create .gitignore files
    faasr_register_workflow_github_create_env(server,repo)
    cli_alert_success("Create github env files")
    # create the payload file
    faasr_register_workflow_github_create_payload(faasr,repo)
    cli_alert_success("Create github payload file")
    # create the README file
    faasr_register_workflow_github_create_readme(repo)
    cli_alert_success("Create github REAME file")
    # create yaml file / default container image is ghcr.io/FaaSr/github-actions-tidyverse:latest
    for (actionname in repo_list[[server]]){
      faasr_register_workflow_github_create_yml_file(faasr,actionname,repo, cron, runner)
    }
    cli_alert_success("Create github workflow yml file")
    cli_progress_update()
    
    # build local repositories
    faasr_register_workflow_git_local_repo(repo,ref)
    cli_alert_success("Create github local repository")
    cli_progress_update()
    
    # build & push remote repositories
    result <- faasr_register_workflow_git_remote_repo(token,response,private,repo,ref)
    if (result==0){
      cli_alert_success("Create github remote repository")
      cli_progress_update()
    } else{
      cli_alert_danger("Error: Failed to update the remote repository")
      stop()
    }
    
    # set environments(variables, secrets)
    faasr_register_workflow_git_remote_env(repo, cred, token)
    cli_progress_update()
    cli_progress_done()
  }
  cli_text(col_cyan("{symbol$menu} {.strong Successfully registered all github actions}"))
}

#' @title faasr_httr_request
#' @description 
#' the help function to send the curl request to the github
#' by using the "httr" library. 
#' @param token a string for the github token
#' @param url a string of url
#' @param body a list of body
#' @param type REST API values; GET/PUT/DELETE/PATCH/POST
#' @return response: response status; 200/202/204/400/etc
#' @import httr
#' @import cli
#' @export

# help sending httr requests
faasr_httr_request <- function(token, url, body=list(), type){
  # get functions depending on "type"
  func <- get(type)
  # write headers
  headers <- c(
    "Accept" = "application/vnd.github+json",
    "Authorization" = paste("Bearer", token),
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  # write body unless it is empty
  if (length(body)!=0){
    body <- jsonlite::toJSON(body, auto_unbox=TRUE)
  }
  # send the REST request(POST/GET/PUT/PATCH)
  response <- func(
    url = paste0("https://api.github.com/", url),
    add_headers(.headers = headers),
    body=body,
    encode="json"
  )
  return(response)
}


#' @title faasr_register_workflow_github_repo_lists
#' @description 
#' Parse the faasr and get the list of function:repository
#' Find actions which is using "Github Actions"
#' return value's key is action and value is server name.
#' @param faasr a list form of the JSON file
#' @return a list of "action:server name" pairs.
#' @import httr
#' @import cli
#' @export

# make a repo list. like a key-value set, key is a server_name and value is a repository name
faasr_register_workflow_github_repo_lists <- function(faasr) {
  # empty list
  repo_list <- list()
  # for each function, iteratively collect servername and repository name
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is Githubactions, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      err_msg <- paste0("\n\n[faasr_msg] invalid server:", server_name," check server type\n\n")
      cli_alert_danger(err_msg)
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "GitHubActions") {
      actionname <- fn
      repo_list[[server_name]] <- unique(c(repo_list[[server_name]],actionname))
    }
  }
  return(repo_list)
}


#' @title faasr_register_workflow_github_repo_exists
#' @description 
#' Check the remote repository is existing on the github
#' by sending the GET request.
#' If it exists, return TRUE, doesn't exist, return FALSE
#' @param faasr_token a string for the github token
#' @param repo a string for the target repository name
#' @return a logical value; if exists, return TRUE, 
#' doesn't exist, return FALSE
#' @import httr
#' @import cli
#' @export

# check github remote repository existence
faasr_register_workflow_github_repo_exists <- function(faasr_token, repo) {
  # get env
  repo <- paste0("repos/",repo)
  # send request to check
  response <- faasr_httr_request(faasr_token, repo, type="GET")
  if (response$status_code == 200) {
    return(TRUE)
  } else if (response$status_code == 404){
    return(FALSE)
  } else {
    cli_alert_danger(" faasr_register_workflow_github_repo_exists: Error - check configurations")
    stop()
  }
}


#' @title faasr_register_workflow_github_repo_question
#' @description 
#' Ask users to update the repository
#' @param check a logical value 
#' @param repo a string for the target repository name
#' @return a logical value to make repository private or not
#' @export

faasr_register_workflow_github_repo_question <- function(check, repo){
  # get env
  private <- TRUE
  # if no remote repo, build a new repository
  if (check==FALSE) {
    # Ask user for the repository to be private or public
    cli_text("{symbol$fancy_question_mark}Enter repository visibility[private/public]")
    while(TRUE) {
      check <- invisible(readline())
      if (check == "private" || check == "") {
        private <- TRUE
        break
      } else if(check == "public") {
        private <- FALSE
        break
      } else {
        cli_alert_warning("Enter \"private\" or \"public\": ")
      }
    }
    # if yes, update the repository
  } else {
    # Ask user for the repository to be updated
    cli_alert_info("Repository already exists")
    cli_text("{symbol$fancy_question_mark} Update the repository?[y/n]")
    while(TRUE) {
      check1 <- readline()
      if (check1=="y" || check1=="") {
        break
      } else if(check1 == "n") {
        cli_alert_danger("Stop the function")
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
      }
    }
  }
  return(private)
}


#' @title faasr_register_workflow_github_create_dir
#' @description 
#' Create the directory for the workflows
#' @param server a string for the server name
#' @param repo a string for the target repository name
#' @export

faasr_register_workflow_github_create_dir <- function(server,repo){
  cwd <- getwd()
  setwd(faasr_gh_local_repo)
  # create directories
  if (dir.exists(repo)) {
    unlink(repo, recursive=TRUE)
  }
  dir.create(repo, recursive=TRUE)
  if (!dir.exists(paste0(repo,"/.github/workflows"))) {
    dir.create(paste0(repo,"/.github/workflows"), recursive=TRUE)
  }
  setwd(cwd)
}


#' @title faasr_register_workflow_github_create_env
#' @description 
#' Create the environment file .gitignore
#' @param server a string for the server name
#' @param repo a string for the target repository name
#' @export

faasr_register_workflow_github_create_env <- function(server,repo){
  # create a file ".gitignore"
  writeLines(paste0(".env\n*~\n*.swp\n*.swo\n.Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n",
                    ".DS_Store\ncache\n*.o\n*.so\n",faasr_data,"\n",faasr_gh_local_repo),
             paste0(faasr_gh_local_repo,"/",repo,"/.gitignore"))
}


#' @title faasr_register_workflow_github_create_payload
#' @description 
#' Create the payload JSON file named "payload.json"
#' @param faasr a list form of the JSON file
#' @param repo a string for the target repository name
#' @export

faasr_register_workflow_github_create_payload <- function(faasr, repo){
  # create a file named "payload.json"
  faasr_gh <- jsonlite::toJSON(faasr, auto_unbox=TRUE)
  faasr_gh_pt <- jsonlite::prettify(faasr_gh)
  write(faasr_gh_pt, paste0(faasr_gh_local_repo,"/",repo,"/payload.json"))
}


#' @title faasr_register_workflow_github_create_readme
#' @description 
#' create README.md file for repository description
#' @param repo a string for the target repository name
#' @export

# create README.md file for repository description
faasr_register_workflow_github_create_readme <- function(repo){
  # create a repository description
  contents <- paste0("# This is an automatically generated FaaSr repository
This repository has been created automatically by the FaaSr register_workflow() function.</br>
It stores the workflow .yml files for your FaaSr workflow, as well as the JSON configuration and secrets.</br>
It is safe to delete this repository if you no longer need this workflow. It can be re-created by running register_workflow()</br>")
  # create a README file
  path <- paste0(faasr_gh_local_repo,"/",repo,"/README.md")
  writeLines(contents, path)
}


#' @title faasr_register_workflow_github_create_yml_file
#' @description 
#' Create a yaml workflow file with the container name
#' @param faasr a list form of the JSON file
#' @param actionname a string for the action name
#' @param repo a string for the target repository name
#' @param cron a string for cron data e.g., */5 * * * *
#' @param runner a logical value; enable runner
#' @import httr
#' @import cli
#' @export

# Create a yaml workflow file with the container name
# TBD implement a native workflow pattern
faasr_register_workflow_github_create_yml_file <- function(faasr, actionname, repo, cron=NULL, runner=FALSE){
  # get env
  folder <- faasr$FaaSrLog
  if (is.null(folder)){
    folder <- ""
  }
  id <- faasr$InvocationID
  if (is.null(id)){
    id <- ""
  }
  if (length(faasr$ActionContainers[[actionname]]) == 0 || faasr$ActionContainers[[actionname]] == "") {
    container_name <- basic_gh_image
  } else {
    container_name <- faasr$ActionContainers[[actionname]]
  }
  # check "runner" / "cron" and bring templates from github
  if (runner){
    if (is.null(cron)){
      #contents_git <- readLines(workflow_runner_path)
    } else {
      #contents_git <- readLines(workflow_runner_w_timer_path)
    }
  } else {
    if (is.null(cron)){
      contents_git <- readLines(workflow_basic_path)
    } else {
      contents_git <- readLines(workflow_timer_path)
    }
  }
  # create customized contents by using "glue"
  contents_git <- paste(contents_git, collapse = "\n")
  contents <- glue::glue(contents_git, .open = "<<", .close = ">>")
  if (!endsWith(actionname,".yml")){
    actionname <- paste0(actionname,".yml")
  }
  # create the workflow file
  path <- paste0(faasr_gh_local_repo,"/",repo,"/.github/workflows/",actionname)
  writeLines(contents, path)
}


#' @title faasr_register_workflow_git_local_repo
#' @description 
#' Create a local git repository
#' @param repo a string for the target repository name
#' @param ref a string for the branch of target repository
#' @export

# create git local repository
faasr_register_workflow_git_local_repo <- function(repo,ref){
  cwd <- getwd()
  setwd(paste0(faasr_gh_local_repo,"/",repo))
  
  # create local git repo
  system("git init", ignore.stderr=TRUE, ignore.stdout=TRUE)
  system(paste0("git checkout -B ", ref), ignore.stderr=TRUE, ignore.stdout=TRUE)
  system("git rm -r git rm -r --cached .", ignore.stderr=TRUE, ignore.stdout=TRUE)
  system("git add .", ignore.stderr=TRUE, ignore.stdout=TRUE)
  system("git commit -m \'update repo\'", ignore.stderr=TRUE, ignore.stdout=TRUE)
  setwd(cwd)
}


#' @title faasr_register_workflow_git_remote_repo
#' @description 
#' create / push git remote repository
#' @param token a string for the github token
#' @param check a logical value whether the remote repository exists
#' @param private a logical value to make repository private or not
#' @param repo a string for the target repository name
#' @param ref a string for the branch of target repository
#' @return a integer value for the result
#' @import httr
#' @import cli
#' @export

# create / push git remote repository
faasr_register_workflow_git_remote_repo <- function(token,check,private,repo,ref){
  cwd <- getwd()
  setwd(paste0(faasr_gh_local_repo,"/",repo))
  # get env
  repo_a <- strsplit(repo, "/")
  repo_p <- repo_a[[1]]
  account_id <- repo_p[1]
  repo_name <- repo_p[2]
  # if github doesn't have repository, then create one.
  if (check==FALSE){
    url <- "user/repos"
    body <- list(name=repo_name, private=private)
    response <- faasr_httr_request(body=body, token=token, url=url, type="POST")
    if (response$status_code==201){
      cli_alert_success("Successfully create the repo")
    } else {
      cli_alert_danger("Error: Failed to create the repo")
      setwd(cwd)
      stop()
    }
  }
  # push files to the repository
  check2 <- system(paste0("git push -f https://github.com/", repo, " ", ref),
                   ,ignore.stderr=TRUE, ignore.stdout=TRUE)
  setwd(cwd)
  return(check2)
}


#' @title faasr_register_workflow_git_remote_env
#' @description 
#' set env(secrets and variables) to the remote repository
#' @param repo a string for the target repository name
#' @param cred a list form of the credentials
#' @param token a string for the github token
#' @import httr
#' @import cli
#' @export

# set env(secrets and variables)
faasr_register_workflow_git_remote_env <- function(repo, cred, token){
  cwd <- getwd()
  setwd(paste0(faasr_gh_local_repo,"/",repo))
  # get public key
  url <- paste0("repos/",repo,"/actions/secrets/public-key")
  response <- faasr_httr_request(body=body, token=token, url=url, type="GET")
  if (response$status_code ==200){
    pub_key <- base64enc::base64decode(content(response)$key)
    key_id <- content(response)$key_id
  } else {
    cli_alert_danger("Error: Failed to get the public key")
    setwd(cwd)
    stop()
  }

  # encode key & secrets by using sodium & base64enc: library required
  secrets_json <- jsonlite::toJSON(cred, auto_unbox=TRUE)
  secrets_binary <- charToRaw(secrets_json)
  secrets_enc <- sodium::simple_encrypt(secrets_binary, pub_key)
  secrets <- base64enc::base64encode(secrets_enc)
  # set the repo secret
  url <- paste0("repos/",repo,"/actions/secrets/SECRET_PAYLOAD")
  body <- list(encrypted_value = secrets, key_id=key_id)
  response <- faasr_httr_request(body=body, token=token, url=url, type="PUT")
  if (response$status_code==204 || response$status_code==201){
    cli_alert_success("Successfully set secrets")
  } else {
    cli_alert_danger("Error: Failed to set secrets")
    setwd(cwd)
    stop()
  }


  # set the repo variable
  url <- paste0("repos/",repo,"/actions/variables")
  body <- list(name="PAYLOAD_REPO", value=paste0(repo,'/payload.json'))
  response <- faasr_httr_request(body=body, token=token, url=url, type="POST")
  if (response$status_code==201){
    cli_alert_success("Successfully set variables")
  } else if (response$status_code==409){
    # if variable already exists, update the variable
    url <- paste0("repos/",repo,"/actions/variables/PAYLOAD_REPO")
    response <- faasr_httr_request(body=body, token=token, url=url, type="PATCH")
    if (response$status_code==204){
      cli_alert_success("Successfully set variables")
    } else {
      cli_alert_danger("Error: Failed to set variables")
      setwd(cwd)
      stop()
    }
  } else {
    cli_alert_danger("Error: Failed to set variables")
    setwd(cwd)
    stop()
  }
  setwd(cwd)
}


#' @title faasr_workflow_invoke_github
#' @description 
#' invoke the github actions workflow on the github repository.
#' Async version; does not wait for the result
#' This will be invoked by faasr_workflow_invoke function
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param faas_name a string for the target server name
#' @param actionname a string for the target action name
#' @import httr
#' @import cli
#' @export

# inovke workflow run
faasr_workflow_invoke_github <- function(faasr, cred, faas_name, actionname){

  # define the required variables.
  token <- cred[[paste0(faas_name,"_TOKEN")]]
  input_id <- faasr$InvocationID
  input_faasr_log <- faasr$FaaSrLog
  repo <- paste0(faasr$ComputeServers[[faas_name]]$UserName,"/",faasr$ComputeServers[[faas_name]]$ActionRepoName)
  git_ref <- faasr$ComputeServers[[faas_name]]$Branch
  if (!endsWith(actionname,".yml") && !endsWith(actionname,".yaml")){
    workflow <- paste0(actionname,".yml")
  } else {
    workflow <- actionname
  }

  # send api request
  url <- paste0("repos/", repo, "/actions/workflows/", workflow, "/dispatches")
  body <- list(
    ref = git_ref,
    inputs = list(
      ID = input_id,
      InvokeName = actionname,
      FaaSrLog = input_faasr_log
    )
  )
  response <- faasr_httr_request(body=body, token=token, url=url, type="POST")
  
  # check result
  if (status_code(response) == 204) {
    succ_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Successfully invoked:", actionname, "\n")
    cli_alert_success(succ_msg)
  } else if (status_code(response) == 401) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Authentication failed, check the credentials\n")
    cli_alert_danger(err_msg)
  } else if (status_code(response) == 404) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Cannot find the destination, check the repo name: \"",repo,"\" and workflow name: \"",workflow,"\"\n")
    cli_alert_danger(err_msg)
  } else if (status_code(response) == 422) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Cannot find the destination, check the ref: ", actionname, "\n")
    cli_alert_danger(err_msg)
  } else {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: unknown error happens when invoke next function\n")
    cli_alert_danger(err_msg)
  }
}


#' @title faasr_set_workflow_timer_gh
#' @description 
#' set/unset cron timer for the github actions.
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @param actionname a string for the target action name
#' @param cron a string for cron data e.g., */5 * * * *
#' @param unset a logical value; set timer(FALSE) or unset timer(TRUE)
#' @import httr
#' @import cli
#' @export

# set workflow timer
faasr_set_workflow_timer_gh <- function(faasr,cred,actionname,cron=NULL,unset=FALSE){

  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")
  
  if (unset){
    char <- "Unset"
  } else {
    char <- "Set"
  }

  cli_h1(paste0("{char} github workflow timer"))
  cli_progress_bar(
    format = paste0(
      "FaaSr {pb_spin} Set github workflow timer ",
      "{cli::pb_bar} {cli::pb_percent} [{pb_current}/{pb_total}]   ETA:{pb_eta}"
    ),
    total = 2
  )

  # get env
  faas_name <- faasr$FunctionList[[actionname]]$FaaSServer
  ref <- faasr$ComputeServers[[faas_name]]$Branch
  repo <- paste0(faasr$ComputeServers[[faas_name]]$UserName,"/",faasr$ComputeServers[[faas_name]]$ActionRepoName)
  path <- paste0(faasr_gh_local_repo, "/", repo)
  token <- cred[[paste0(faas_name,"_TOKEN")]]
  folder <- faasr$FaaSrLog
  id <- faasr$InvocationID
  if (!endsWith(actionname,".yml") && !endsWith(actionname,".yaml")){
    workflow <- paste0(actionname,".yml")
  } else {
    workflow <- actionname
  }

  # check local repo
  if (!dir.exists(path)){
    err_msg <- paste0("[faasr_msg] faasr_set_workflow_timer_gh: No local repository ",repo," found \n")
    cli_alert_danger(err_msg)
    stop()
  }
  # check remote repo
  response <- faasr_register_workflow_github_repo_exists(token, repo)
  if (!response){
    err_msg <- paste0("[faasr_msg] faasr_set_workflow_timer_gh: No remote repository ",repo," found \n")
    cli_alert_danger(err_msg)
    stop()
  }

  # update yaml file
  # TBD cron timer for runner
  if (unset){
    faasr_register_workflow_github_create_yml_file(faasr, actionname, repo, cron=NULL, runner=FALSE)
  } else {
    faasr_register_workflow_github_create_yml_file(faasr, actionname, repo, cron=cron, runner=FALSE)
  }

  # build local repositories
  faasr_register_workflow_git_local_repo(repo,ref)
  cli_alert_success("Update local git repository workflow")
  cli_progress_update()

  # build & push remote repositories
  result <- faasr_register_workflow_git_remote_repo(token,response,private=NULL,repo,ref)
  cli_alert_success("Update remote git repository workflow")
  if (result==0){
    cli_alert_success(paste0("Successfully ",char," the cron ",cron," timer to the repository ",repo))
    cli_progress_update()
  } else{
    cli_alert_warning(paste0("Error: Failed to ",char," the cron ",cron," timer to the repository ",repo))
    stop()
  }
  cli_progress_done()
}
