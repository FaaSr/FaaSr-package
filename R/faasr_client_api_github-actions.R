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
faasr_register_workflow_github_actions <- function(faasr, cred, cron=NULL, runner=FALSE) {

  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")

  # get a repo list
  repo_list <- faasr_register_workflow_github_repo_lists(faasr)
  for (server in names(repo_list)) {
    # get env
    faasr_token <- cred[[paste0(server,"_TOKEN")]]
    ref <- faasr$ComputeServers[[server]]$Branch
    repo <- paste0(faasr$ComputeServers[[server]]$UserName,"/",faasr$ComputeServers[[server]]$ActionRepoName)
    
    cli_h1(paste0("Registering workflow github actions for repo: ", server))
    cli_progress_bar(
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
    response <- faasr_register_workflow_github_repo_exists(faasr_token,repo)
    # check user's request
    private <- faasr_register_workflow_github_repo_question(response)
    # create directories
    faasr_register_workflow_github_create_dir(server,repo,cred)
    cli_alert_success("Create github local directories")
    # create .gitignore files
    faasr_register_workflow_github_create_env(server,repo,cred)
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
    faasr_register_workflow_git_local_repo(response,repo,ref)
    cli_alert_success("Create github local repository")
    cli_progress_update()
    # build & push remote repositories
    result <- faasr_register_workflow_git_remote_repo(token,response,private,repo,ref)
    if (result==0){
      cli_alert_success("Create github remote repository")
      cli_progress_update()
      #cat("\n\n[faasr_msg] Successfully update the remote repository\n")
    } else{
      cli_alert_danger("Error: Failed to update the remote repository")
      #cat("\n\n[faasr_msg] Error: Failed to update the remote repository\n")
      stop()
    }
    # set environments(variables, secrets)
    faasr_register_workflow_git_remote_env(repo, cred, token)
    cli_alert_success("Set github environment-secrets/variable")
    cli_progress_update()
    cli_progress_done()
    #cat("\n\n[faasr_msg] successfully registed server: ", repo,"\n\n")
  }
  cli_text(col_cyan("{symbol$menu} {.strong Successfully registed all github actions}"))
  #cat("\n\n[faasr_msg] successfully registed all servers\n\n")
}
# help sending httr requests
faasr_httr_request <- function(token, url, body=list(), type){
  library("httr")
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
# make a repo list. like a key-value set, key is a server_name and value is a repository name
faasr_register_workflow_github_repo_lists <- function(faasr) {
  # empty list
  repo_list <- list()
  # for each function, iteratively collect servername and repository name
  for (fn in names(faasr$FunctionList)) {
    server_name <- faasr$FunctionList[[fn]]$FaaSServer
    # if FaaStype is Githubactions, add it to the list
    if (is.null(faasr$ComputeServers[[server_name]]$FaaSType)){
      #cat("\n\n[faasr_msg] invalid server:", server_name," check server type\n\n")
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "GitHubActions") {
      actionname <- fn
      repo_list[[server_name]] <- unique(c(repo_list[[server_name]],actionname))
    }
  }
  return(repo_list)
}
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
    #cat("\n\n[faasr_msg] faasr_register_workflow_github_repo_exists: Error - check configurations\n\n")
    stop()
  }
}
faasr_register_workflow_github_repo_question <- function(check, repo){
  # get env
  private <- TRUE
  # if no remote repo, build a new repository
  if (check==FALSE) {
    # Ask user for the repository to be private or public
    cli_text("{symbol$fancy_question_mark}Enter repository visibility[private/public]")
    #cat("[private/public]")
    while(TRUE) {
      check <- invisible(readline())
      if (check == "private") {
        private <- TRUE
        break
      } else if(check == "public") {
        private <- FALSE
        break
      } else {
        cli_alert_warning("Enter \"private\" or \"public\": ")
        #cat("Enter \"private\" or \"public\": \n")
      }
    }
    # if yes, update the repository
  } else {
    # Ask user for the repository to be updated
    cli_alert_info("Repository already exists")
    cli_text("{symbol$fancy_question_mark} Update the repository?[y/n]")
    #cat("\n\n[faasr_msg] Repository already exists\n")
    #cat("[faasr_msg] Update the repository?[y/n]")
    while(TRUE) {
      check1 <- invisible(readline())
      if (check1=="y") {
        break
      } else if(check1 == "n") {
        cli_alert_danger("Stop the function")
        #cat("\n\n[faasr_msg] Stop the function\n")
        stop()
      } else {
        cli_alert_warning("Enter \"y\" or \"n\": ")
        #cat("Enter \"y\" or \"n\": ")
      }
    }
  }
  return(private)
}
faasr_register_workflow_github_create_dir <- function(server,repo,cred){
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
faasr_register_workflow_github_create_env <- function(server,repo,cred){
  # create a file ".gitignore"
  writeLines(paste0(".env\n*~\n*.swp\n*.swo\n.Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n",
                    ".DS_Store\ncache\n*.o\n*.so\n",faasr_data,"\n",faasr_gh_local_repo),
             paste0(faasr_gh_local_repo,"/",repo,"/.gitignore"))
}
faasr_register_workflow_github_create_payload <- function(faasr, repo){
  # create a file named "payload.json"
  faasr_gh <- jsonlite::toJSON(faasr, auto_unbox=TRUE)
  faasr_gh_pt <- jsonlite::prettify(faasr_gh)
  write(faasr_gh_pt, paste0(faasr_gh_local_repo,"/",repo,"/payload.json"))
}
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
# Create a yaml workflow file with the container name
# TBD implement a native workflow pattern
faasr_register_workflow_github_create_yml_file <- function(faasr, actionname, repo, cron=NULL, runner=FALSE){
  # get env
  folder <- faasr$FaaSrLog
  id <- faasr$InvocationID
  if (length(faasr$ActionContainers[[actionname]]) == 0 || faasr$ActionContainers[[actionname]] == "") {
    container_name <- basic_gh_image
  } else {
    container_name <- faasr$ActionContainers[[actionname]]
  }
  # check "runner" / "cron" and bring templates from github
  if (runner){
    if (is.null(cron)){
      #contents_git <- readLines("runner yaml url")
    } else {
      #contents_git <- readLines("runner with cron yaml url")
    }
  } else {
    if (is.null(cron)){
      contents_git <- readLines("https://raw.githubusercontent.com/spark0510/FaaSr-package/branch35-trial/schema/workflow_template.yml")
    } else {
      contents_git <- readLines("https://raw.githubusercontent.com/spark0510/FaaSr-package/branch35-trial/schema/workflow_with_cron_template.yml")
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
# create git local repository
faasr_register_workflow_git_local_repo <- function(check,repo,ref){
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
      #cat("\n\n[faasr_msg] Successfully create the repo\n")
    } else {
      cli_alert_danger("Error: Failed to create the repo")
      #cat("\n\n[faasr_msg] Error: Failed to create the repo\n")
      setwd(cwd)
      stop()
    }
  }
  # push files to the repository
  check2 <- system(paste0("git push -f http://github.com/", repo, " ", ref),
                   ,ignore.stderr=TRUE, ignore.stdout=TRUE)
  setwd(cwd)
  return(check2)
}
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
    #cat("\n\n[faasr_msg] Error: Failed to get the public key\n")
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
    #cat("\n\n[faasr_msg] Successfully set secrets\n")
  } else {
    cli_alert_danger("Error: Failed to set secrets")
    #cat("\n\n[faasr_msg] Error: Failed to set secrets\n")
    setwd(cwd)
    stop()
  }
  # set the repo variable
  url <- paste0("repos/",repo,"/actions/variables")
  body <- list(name="PAYLOAD_REPO", value=paste0(repo,'/payload.json'))
  response <- faasr_httr_request(body=body, token=token, url=url, type="POST")
  if (response$status_code==201){
    cli_alert_success("Successfully set variables")
    #cat("\n\n[faasr_msg] Successfully set variables\n")
  } else if (response$status_code==409){
    # if variable already exists, update the variable
    url <- paste0("repos/",repo,"/actions/variables/PAYLOAD_REPO")
    response <- faasr_httr_request(body=body, token=token, url=url, type="PATCH")
    if (response$status_code==204){
      cli_alert_success("Successfully set variables")
      #cat("\n\n[faasr_msg] Successfully set variables\n")
    } else {
      cli_alert_danger("Error: Failed to set variables")
      #cat("\n\n[faasr_msg] Error: Failed to set variables\n")
      setwd(cwd)
      stop()
    }
  } else {
    cli_alert_danger("Error: Failed to set variables")
    #cat("\n\n[faasr_msg] Error: Failed to set variables\n")
    setwd(cwd)
    stop()
  }
  setwd(cwd)
}
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
    #cat(succ_msg)
  } else if (status_code(response) == 401) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Authentication failed, check the credentials\n")
    cli_alert_danger(err_msg)
    #cat(err_msg)
  } else if (status_code(response) == 404) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Cannot find the destination, check the repo name: \"",repo,"\" and workflow name: \"",workflow,"\"\n")
    cli_alert_danger(err_msg)
    #cat(err_msg)
  } else if (status_code(response) == 422) {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: Cannot find the destination, check the ref: ", actionname, "\n")
    cli_alert_danger(err_msg)
    #cat(err_msg)
  } else {
    err_msg <- paste0("faasr_register_workflow_github_invoke: GitHub Action: unknown error happens when invoke next function\n")
    cli_alert_danger(err_msg)
    #cat(err_msg)
  }
}
# set workflow timer
faasr_set_workflow_timer_gh <- function(faasr,cred,actionname,cron=NULL,unset=FALSE){
  
  options(cli.progress_clear = FALSE)
  options(cli.spinner = "line")
  
  cli_h1(paste0("Set github workflow timer"))
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
    #cat(err_msg)
    stop()
  }
  # check remote repo
  response <- faasr_register_workflow_github_repo_exists(token, repo)
  if (!response){
    err_msg <- paste0("[faasr_msg] faasr_set_workflow_timer_gh: No remote repository ",repo," found \n")
    cli_alert_danger(err_msg)
    #cat(err_msg)
    stop()
  }
  # update yaml file
  # TBD cron timer for runner
  if (unset){
    faasr_register_workflow_github_create_yml_file(faasr, actionname, repo, cron=NULL, runner=FALSE)
    char <- "unset"
  } else {
    faasr_register_workflow_github_create_yml_file(faasr, actionname, repo, cron=cron, runner=FALSE)
    char <- "set"
  }
  # build local repositories
  faasr_register_workflow_git_local_repo(response,repo,ref)
  cli_alert_success("Update local git repository workflow")
  cli_progress_update()
  # build & push remote repositories
  result <- faasr_register_workflow_git_remote_repo(token,response,private=NULL,repo,ref)
  cli_alert_success("Update remote git repository workflow")
  if (result==0){
    cli_alert_success(paste0("Successfully ",char," the cron ",cron," timer to the repository ",repo))
    cli_progress_update()
    #cat("\n\n[faasr_msg] Successfully",char,"the cron",cron,"timer to the repository",repo,"\n")
  } else{
    cli_alert_warning(paste0("Error: Failed to ",char," the cron ",cron," timer to the repository ",repo))
    #cat("\n\n[faasr_msg] Error: Failed to",char,"the cron",cron,"timer to the repository",repo,"\n")
    stop()
  }
  cli_progress_done()
}
