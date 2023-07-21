#' @title Interactive function to register a workflow with GitHub Actions
#' @description This script automates the process of using GitHub Actions command-line interface tools to register
#'              the various actions that are part of a user's workflow with GitHub Actions
#'              The script depends on the user having the git and gh command-line tool installed in their computer:
#'              The script expects a command-line argument that is the name of a FaaSr-compliant JSON configuration
#'              If successful, the script registers all the actions declared in the workflow bound to the
#'              Docker containers also declared in the workflow
#' @param payload_file name of the JSON payload file

faasr_register_workflow_github-actions <- function(payload_file) {
  faasr<-jsonlite::fromJSON(payload_file)

  # get repository list
  repo_list <- faasr_register_workflow_github_repo_lists(faasr)

  # create repository iteratively
  for (server in names(repo_list)) {
    # login server with the given server's token
    file_name <- paste0(server,"_token.txt")
    writeLines(faasr$ComputeServers[[server]]$Token, file_name)
    system(paste0("gh auth login --with-token < ",file_name))
    file.remove(file_name)
    for (repo in repo_list[[server]]) {
      # check the repository
      response <- faasr_register_workflow_github_repo_exists(repo)
      faasr_register_workflow_github_create_env(server,repo,faasr)
      faasr_register_workflow_github_set_payload(faasr)
      if (length(faasr$Actioncontainer[[repo]]) == 0) {
        faasr_register_workflow_github_create_yml_file("faasr/base-tidyverse-github",faasr)
      } else {
        container_name <- faasr$Actioncontainer[[repo]]
        faasr_register_workflow_github_create_yml_file(container_name,faasr)
      }
      faasr_register_workflow_github_gh_setup(response, repo)
    }
  }
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
    if (faasr$ComputeServers[[server_name]]$FaaSType == "GitHubActions") {
      repo_name <- faasr$FunctionList[[fn]]$Actionname
      repo_list[[server_name]] <- unique(c(repo_list[[server_name]],repo_name))
    }
  }
  return(repo_list)
}

# Create an environment
faasr_register_workflow_github_create_env <- function(server_name, repo_name, faasr) {
  # split repo into account id and repository name
  repo <- strsplit(repo_name, "/")
  repo_p <- repo[[1]]
  account_id <- repo_p[1]
  repo_name_only <- repo_p[2]
  # create a directory named repository named
  if (!dir.exists(repo_name_only)) {
    dir.create(repo_name_only, recursive=TRUE)
  } else {
    cat("Directory for the repository already exists\n")
    cat("Update?[y/n]")
    while(TRUE) {
      check <- readLines(con="stdin", 1)
      if (check == "y") {
        break
      } else if(check == "n") {
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
  }
  # set working directory
  setwd(repo_name_only)
  # prepare arguments
  secret_github <- paste0("{\"PAYLOAD_GITHUB_TOKEN\":\"",faasr$ComputeServers[[server_name]]$Token)
  secrets <- list()
  secrets$PAYLOAD_GITHUB_TOKEN<-faasr$ComputeServers[[server_name]]$Token
  secrets$GITHUB_S3_ACCESS_KEY<-faasr$DataStores[[faasr$LoggingServer]]$AccessKey
  secrets$GITHUB_S3_SECRET_KEY<-faasr$DataStores[[faasr$LoggingServer]]$SecretKey
  secrets_json <- jsonlite::toJSON(secrets, auto_unbox=TRUE)
  contents <- paste0(docker_id,"\n",docker_pw,"\n","SECRET_PAYLOAD=",secrets_json)
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
  # Hide real keys
  for (server in names(faasr_gh$ComputeServers)) {
    if (faasr_gh$ComputeServers[[server]]$FaaSType=="GitHubActions") {
      faasr_gh$ComputeServers[[server]]$Token <- "PAYLOAD_GITHUB_TOKEN"
    } else if (faasr_gh$ComputeServers[[server]]$FaaSType=="OpenWhisk") {
      faasr_gh$ComputeServers[[server]]$API.key <- "GITHUB_OW_API_KEY"
    } else if (faasr_gh$ComputeServers[[server]]$FaaSType=="Lambda") {
      faasr_gh$ComputeServers[[server]]$AccessKey <- "GITHUB_Lambda_ACCESS_KEY"
      faasr_gh$ComputeServers[[server]]$SecretKey <- "GITHUB_Lambda_SECRET_KEY"
    }
  }
  for (data in names(faasr_gh$DataStores)) {
    faasr_gh$DataStores[[data]]$AccessKey <- "GITHUB_S3_ACCESS_KEY"
    faasr_gh$DataStores[[data]]$SecretKey <- "GITHUB_S3_SECRET_KEY"
  }
  # create a file named "payload.json"
  faasr_gh <- jsonlite::toJSON(faasr_gh, auto_unbox=TRUE)
  write(faasr_gh, "payload.json")
}

# Create a yaml workflow file with the container name
faasr_register_workflow_github_create_yml_file <- function(containername, faasr){
  contents <- paste0("name: Run Docker Image from Docker Hub

on:
  workflow_dispatch:
    inputs:
      ID:
        description: 'ID'
        required: false
      InvokeName:
        description: 'FunctionInvoke'
        required: true

jobs:
  run_docker_image:
    runs-on: ubuntu-latest
    env:
      SECRET_PAYLOAD: ${{ secrets.SECRET_PAYLOAD }}
      INPUT_ID: ${{ github.event.inputs.ID }}
      INPUT_INVOKENAME: ${{ github.event.inputs.InvokeName }}
      PAYLOAD_REPO: ${{ vars.PAYLOAD_REPO }}
    steps:
    - name: Login to DockerHub
      uses: docker/login-action@v2
      with:
        username: ${{ secrets.DOCKERHUB_USERNAME }}
        password: ${{ secrets.DOCKERHUB_SECRET }}

    - name: Pull and run Docker image pass env
      run: |
        docker pull ",containername,"\n",
        "        docker run -e SECRET_PAYLOAD -e INPUT_ID -e INPUT_INVOKENAME -e PAYLOAD_REPO ",containername)
  workflow_name<-faasr$ComputeServers[[faasr$FunctionList[[faasr$FunctionInvoke]]$FaaSServer]]$WorkflowName
  path <- paste0(".github/workflows/",workflow_name)
  writeLines(contents, path)
}

# setup a git repository
faasr_register_workflow_github_gh_setup <- function(check, repo) {
  # split repo into account id and repository name
  repo_a <- strsplit(repo, "/")
  repo_p <- repo_a[[1]]
  account_id <- repo_p[1]
  repo_name <- repo_p[2]
  # create a local git repository
  system("git init")
  system("git branch -m main")
  system("git add .")
  # check whether git remote repository already exists
  if (check==FALSE) {
    # if not, build a new repository
    system("git commit -m \'build repo\'")
    cat("Create the repository\n")
    # Ask user for the repository to be private or public
    cat("[private/public]")
    while(TRUE) {
      check <- readLines(con="stdin", 1)
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
    system(paste0("gh repo create ", repo_name, " --",auth," --push --source=."))
  } else if (check == TRUE) {
    # if the repository already exists, ask user to update it or not
    cat("Repository already exists\n")
    cat("Update the repository?[y/n]")
    while(TRUE) {
      check1 <- readLines(con="stdin", 1)
      if (check1=="y") {
        system("git commit -m \'update repo\'")
        command <- paste0("git push -f http://github.com/",repo," main")
        system(command)
        break
      } else if(check1 == "n") {
        cat("stop the function\n")
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
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
  #exit_code <- system2("gh", args = c("repo", "view", repo_info))
  # print("exit_code is:")
  # print(exit_code)
  # if it exists, it returns TRUE, if not, it returns FALSE
  return(exit_code == 0)
}


