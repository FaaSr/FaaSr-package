#' @title Client tool for registering and invoking workflow by using the json file.
#' @description Uses a list to embed given functions
#' @description faasr_register_workflow
#' starts to register functions by reading "json" and "cred"
#' @description faasr_collect_sys_env
#' collects Environment values by using "Sys.getenv()"
#' @description .faasr_get_svc
#' gets "svc" list by searching caller and envrionment.
#' @description faasr
#' reads given json file and turns it into the list. If it contains credentials,
#' it converts those credentials into the representative(servername + key type) and save the real key
#' to the "cred".
#' @description faasr_replace_values
#' convert representative(servername + key type) into the real key.
#' @description faasr_invoke_workflow
#' starts a workflow by invoking very first function.
#' 
#' @param faasr_collect_sys_env: faasr for a list for json, cred for a list for credentials(keys)
#' @param faasr: json_path for a local JSON file path, env_path for a local credentials file path(optional)
#' @param faasr_replace_values: faasr for a list for json, cred for a list for credentials(keys)
#' 
#' @return faasr_collect_sys_env: cred for a list of credentials.
#' @return .faasr_get_svc: "object" for a list containing "operations", "json", "json_path", and "cred" of caller
#' @return faasr: "svc" for a list containing "operations", "json", "json_path", and "cred"
#' @return faasr_replace_values: "faasr" for a list containing both json structure and credentials.

library("cli")

# define a list for storing functions
.faasr_user <- list()

# defined variables
faasr_gh_local_repo <- "faasr_gh_local_repo"
faasr_data <- "faasr_data"
basic_gh_image <- "ghcr.io/faasr/github-actions-tidyverse:latest"
basic_ow_image <- "faasr/openwhisk-tidyverse:latest"
basic_ld_image <- "145342739029.dkr.ecr.us-east-1.amazonaws.com/aws-lambda-tidyverse:latest"

# faasr_register_workflow function
faasr_register_workflow <- function(...){
  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  
  setwd(faasr_wd)

  # register actions for openwhisk/github-actions/lambda by given json
  check <- faasr_register_workflow_openwhisk(faasr,cred,...)
  check <- faasr_register_workflow_github_actions(faasr,cred)
  #faasr_register_workflow_aws_lambda(faasr,cred)
  
}
.faasr_user$operations$register_workflow <- faasr_register_workflow


# collect system envrionment
faasr_collect_sys_env <- function(faasr, cred){
  # Check the computeservers.
  for (faas_cred in names(faasr$ComputeServers)){
    # if it is github actions, use key type for "Token"
    # if given cred_name(servername + key type) is empty, set it up
    # if "cred" doesn't have a value, use "Sys.getenv" to get the real key.
    # if "Sys.getenv" value is empty, return an error message
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
    
    # if it is openwhisk, use key type for "API.key"
    # if given cred_name(servername + key type) is empty, set it up
    # if "cred" doesn't have a value, use "Sys.getenv" to get the real key.
    # if "Sys.getenv" value is empty, return an error message
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
      
    # if it is Lambda, use key types for "AccessKey" and "SecretKey"
    # if given cred_name(servername + key type) is empty, set it up
    # if "cred" doesn't have a value, use "Sys.getenv" to get the real key.
    # if "Sys.getenv" value is empty, return an error message
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
  
  # Check the DataStores.
  for (data_cred in names(faasr$DataStores)){
    # Use key types for "AccessKey" and "SecretKey"
    # if given cred_name(servername + key type) is empty, set it up
    # if "cred" doesn't have a value, use "Sys.getenv" to get the real key.
    # if "Sys.getenv" value is empty, return an error message
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

# get the list "svc"
.faasr_get_svc <- function(){
  # get the caller's envrionments.
  calling_env <- parent.frame(2)
  # get the caller
  call <- sys.call(-1)[[1]]
  # if caller is a function, get one more step to get a caller for a list
  if (is.function(call)) {
    call <- sys.call(-2)[[2]]
  }
  # merge and get the caller
  object <- eval(call[[2]], envir = calling_env)
  return(object)
}

# faasr main function
faasr <- function(json_path=NULL, env_path=NULL){

  if (json_path=="" || is.null(json_path)){
    cat("[faasr msg] JSON path is required")
  }

  # set the svc and environments
  faasr_wd <- getwd()
  svc <- .faasr_user$operations
  svc$cred <- list()
  svc$json <- jsonlite::fromJSON(json_path)
  svc$path <- list(json=json_path, env=env_path, id=paste0("faasr_",sample(100000, size=1)))
  svc$wd <- faasr_wd
  
  # Check the ComputeServers
  for (faas_js in names(svc$json$ComputeServers)){
    switch (svc$json$ComputeServers[[faas_js]]$FaaSType,
            # If it is GithubActions and key is given by JSON file, replace it into representative(servername+token)
            # Real key will be stored in the cred
            "GitHubActions"={
              if (!is.null(svc$json$ComputeServers[[faas_js]]$Token)){
                if (svc$json$ComputeServers[[faas_js]]$Token != paste0(faas_js,"_TOKEN")){
                  svc$cred[[paste0(faas_js,"_TOKEN")]] <- svc$json$ComputeServers[[faas_js]]$Token
                }
              }
              svc$json$ComputeServers[[faas_js]]$Token <- paste0(faas_js,"_TOKEN")
            },
            # If it is Lambda and key is given by JSON file, replace it into representative(servername+token)
            # Real key will be stored in the cred
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
            # If it is Openwhisk and key is given by JSON file, replace it into representative(servername+token)
            # Real key will be stored in the cred
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
  # Check the Datastores
  # Keys are given by JSON file, replace them into representative(servername+token)
  # Real keys will be stored in the cred
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
  
  # if env_path is given, it would read the envrionment values from the path 
  if (!is.null(env_path)){
    envs <- readLines(env_path, warn=FALSE)
    
    for (env in envs){
      # each key:value pair is divided by "=", so it should parse them.
      # save the key:value as a list
      env_parts <- strsplit(env, "=")
      if (length(env_parts[[1]]) == 2) {
        env_key <- trimws(gsub("[\"]", "", env_parts[[1]][1]))
        env_value <- trimws(gsub("[\"\",]", "", env_parts[[1]][2]))
        svc$cred[[env_key]] <- env_value
      }
    }
  }
  
  # create dir
  if (!dir.exists(faasr_gh_local_repo)){
    dir.create(faasr_gh_local_repo)
  }
  if (!dir.exists(faasr_data)){
    dir.create(faasr_data)
  }

  return(svc)
}


# replace fake values into real values
faasr_replace_values <- function(faasr, cred){
  for (name in names(faasr)) {
    # skip the FunctionList/FunctionGitRepo/FunctionCRANPackage/FunctionGitHubPackage
    if (name == "FunctionList" || name=="FunctionGitRepo" || name == "FunctionCRANPackage" || name == "FunctionGitHubPackage") {
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

# invoke first action
faasr_invoke_workflow <- function(FunctionInvoke=NULL, ...){
  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  
  setwd(faasr_wd)
  
  # if there's given function, it will be invoked.
  if (!is.null(FunctionInvoke)){
    actionname <- FunctionInvoke
  } else{
    actionname <- faasr$FunctionInvoke
  }
  # define the required variables.
  faas_name <- faasr$FunctionList[[actionname]]$FaaSServer
  faas_type <- faasr$ComputeServers[[faas_name]]$FaaSType

  switch(faas_type,
         # If first action is github actions, use github
         "GitHubActions"={
            faasr_workflow_invoke_github(faasr, cred, faas_name, actionname)
         },
         # If first action is aws-lambda, use lambda
         "Lambda"={
           # json file with credentials will be created and after invocation, it will be removed.
           faasr_w_cred <- faasr_replace_values(faasr, cred)
           faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
           rd_nb <- sample(100000, size=1)
           writeLines(faasr_json, paste0("payload_ld_",rd_nb,".json"))

           # get lambda function timeout
           check_lambda_config_command <- paste0("aws lambda get-function-configuration --function-name ", actionname)
           check_lambda_config_result <- system(check_lambda_config_command, intern = TRUE)

           status_code <- attr(check_lambda_config_result, "status")
           if (!is.null(status_code) && status_code == 254){
             lambda_func_time_out <- 120
           } else {
            json_string <- paste(check_lambda_config_result, collapse = "")
            json_data <- jsonlite::fromJSON(json_string)
            lambda_func_time_out <- json_data$Timeout
          }
          
           command <- paste0("aws lambda invoke --function-name ",actionname," --cli-connect-timeout ", lambda_func_time_out ," --cli-binary-format raw-in-base64-out --invocation-type RequestResponse --payload file://",paste0("payload_ld_",rd_nb,".json")," lambda_outputfile.txt")
           check <- system(command)
           file.remove(paste0("payload_ld_",rd_nb,".json"))
         },
         # If first action is openwhisk, use ibmcloud
         "OpenWhisk"={
           faasr_workflow_invoke_openwhisk(faasr, cred, faas_name, actionname, ...)
         })
}
.faasr_user$operations$invoke_workflow <- faasr_invoke_workflow

# set the cron timer
faasr_set_workflow_timer <- function(cron, target=NULL, ...){

  # get env
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  if (is.null(target)){
    target <- faasr$FunctionInvoke
  }

  setwd(faasr_wd)

  if (is.null(cron) || cron==""){
    cat("\n\n[faasr_msg] No cron time provided\n")
    stop()
  }

  # set timer depending on the faas providers
  type <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$FaaSType
  if (type == "GitHubActions"){
    faasr_set_workflow_timer_gh(faasr,cred,target,cron)
  } else if (type == "Lambda"){
    faasr_set_workflow_timer_ld(faasr,cred,target,cron)
  } else if (type == "OpenWhisk"){
    faasr_set_workflow_timer_ow(faasr,cred,target,cron, ...)
  }
}
.faasr_user$operations$set_workflow_timer <- faasr_set_workflow_timer

# unset the timer
faasr_unset_workflow_timer <- function(target=NULL,...){

  # get env
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)

  setwd(faasr_wd)

  if (is.null(target)){
    target <- faasr$FunctionInvoke
  }

  # unset the timer depending on the faas providers
  type <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$FaaSType
  if (type == "GitHubActions"){
    faasr_set_workflow_timer_gh(faasr,cred, target, cron=NULL, unset=TRUE)
  } else if (type == "Lambda"){
    faasr_set_workflow_timer_ld(faasr,cred, target, cron=NULL, unset=TRUE)
  } else if (type == "OpenWhisk"){
    faasr_set_workflow_timer_ow(faasr,cred,target, cron=NULL, unset=TRUE,...)
  }
}
.faasr_user$operations$unset_workflow_timer <- faasr_unset_workflow_timer

# save the config(instance)
faasr_save_config <- function(file_name=NULL){

  # get env
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  if (is.null(file_name)){
    file_name <- svc$path$id
  }

  # current working directory should have "faasr_data" folder
  if (!dir.exists(faasr_data)){
    cat("\n\n[faasr_msg] No faasr_data folder found\n")
    stop()
  }

  setwd(faasr_wd)

  cat("\n\n[faasr_msg] Warning: configuration including SECRET will be stored in \"faasr_data\" folder\n")

  # get env / name would only use file name (not whole path)
  json_name <- basename(svc$path$json)
  ctime <- Sys.time()
  file_path <- paste0(faasr_data, "/", json_name, "/", file_name, ".rds")
  if (!dir.exists(paste0(faasr_data, "/", json_name))){
    dir.create(paste0(faasr_data, "/", json_name), recursive=TRUE)
  }
  # save the instance as a form of RDS
  saveRDS(svc, file_path)

  cat("\n\n[faasr_msg] Successfully save the instance, file path:",file_path,"\n")
}
.faasr_user$operations$save_config <- faasr_save_config

# get config(instance) from "faasr_data" folder
faasr_get_config <- function(json_name=NULL){

  # current working directory should have "faasr_data" folder
  if (!dir.exists(faasr_data)){
    cat("\n\n[faasr_msg] No faasr_data folder found\n")
    stop()
  }
  
  # if no json_name is provided, get all RDS files.
  if (is.null(json_name)){
    file_list <- list.files(faasr_data)
  } else {
    file_list <- basename(json_name)
  }
  for (files in file_list){
    rds_list <- list.files(paste0(faasr_data,"/",files))
    if (length(rds_list) == 0 ) {
      cat("\n[faasr_msg] No configurations in repo", files,"\n")
    }

    # get rds files / user defines variable name, defualt is its file name
    for (rds in rds_list){
      var_name <- gsub("\\.rds$", "", rds)
      cat("\n[faasr_msg] Loading ",files,"/",var_name,", Please provide variable name:(\"stop\" to stop)\n")
      while(TRUE) {
        check <- readline()
        if (!grepl("^\\.?[[:alpha:].][[:alnum:].]*$", check)) {
          cat("\n[faasr_msg] Invalid variable name, please privode the valid name\n")
        } else if (check == "stop"){
          stop()
        } else {
          break
        }
      }

      # read rds / save it as a global variable
      rds_data <- readRDS(paste0(faasr_data,"/",files,"/",rds))
      assign(check, rds_data, envir = .GlobalEnv)
      # once read, file will be deleted
      file.remove(paste0(faasr_data,"/",files,"/",rds))
    }
  }
}
