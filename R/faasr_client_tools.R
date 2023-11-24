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

# define a list for storing functions
.faasr_user <- list()

# faasr_register_workflow function
faasr_register_workflow <- function(){
  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  cred <- svc$cred
  faasr <- svc$json
  json_path <- svc$json_path
  cred <- faasr_collect_sys_env(faasr,cred)
  
  # register actions for openwhisk/github-actions/lambda by given json
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
  
  # update the json and write an updated json file into the json path.
  json_update <- jsonlite::toJSON(faasr, pretty=TRUE, auto_unbox=TRUE)
  writeLines(json_update, json_path)
  
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
faasr <- function(json_path, env_path=NULL){
  # set the svc and environments
  wd <- getwd()
  svc <- .faasr_user$operations
  svc$cred <- list()
  svc$json <- jsonlite::fromJSON(json_path)
  
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
  
  # create .faasr_json for saving the json.
  if (!dir.exists(".faasr_json")){
    dir.create(".faasr_json")
  }
  # create a random number
  rd_nb <- sample(1:1000000, size=1)
  # define a json path
  json_path <- paste0(wd,"/.faasr_json/.faasr_json_",rd_nb)
  json_update <- jsonlite::toJSON(svc$json, pretty=TRUE, auto_unbox=TRUE)
  # write a json file with json_path
  writeLines(json_update, json_path)
  # save the json_path into the svc list.
  svc$json_path <- json_path
  
  return(svc)
}


# replace fake values into real values
faasr_replace_values <- function(faasr, cred){
  for (name in names(faasr)) {
    # skip the FunctionList
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

# invoke first action
faasr_invoke_workflow <- function(FunctionInvoke=NULL){
  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  cred <- svc$cred
  json_path <- svc$json_path
  faasr <- jsonlite::fromJSON(json_path)
  cred <- faasr_collect_sys_env(faasr,cred)
  
  # if there's given function, it will be invoked.
  if (!is.null(FunctionInvoke)){
    actionname <- FunctionInvoke
  } else{
    actionname <- faasr$FunctionInvoke
  }
  # define the required variables.
  faas_name <- faasr$FunctionList[[actionname]]$FaaSServer
  faas_type <- faasr$ComputeServers[[faas_name]]$FaaSType
  functionname <- faasr$FunctionList[[actionname]]$FunctionName
  
  switch(faas_type,
         # If first action is github actions, use github
         "GitHubActions"={
           if (!endsWith(actionname,".yml") && !endsWith(actionname,".yaml")){
            actionname_yml <- paste0(actionname,".yml")
           }
           gh_ref <- faasr$ComputeServers[[faas_name]]$Branch
           repo <- paste0(faasr$ComputeServers[[faas_name]]$UserName,"/",faasr$ComputeServers[[faas_name]]$ActionRepoName)
           command <- paste0("gh workflow run --repo ",repo," --ref ",gh_ref," ",actionname_yml," -f InvokeName=",actionname)
           check <- system(command)
         },
         "Lambda"={
           # If first action is aws-lambda, use lambda
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
         "OpenWhisk"={
           # If first action is openwhisk, use ibmcloud
           # json file with credentials will be created and after invocation, it will be removed.
           faasr_w_cred <- faasr_replace_values(faasr, cred)
           faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
           rd_nb <- sample(100000, size=1)
           writeLines(faasr_json, paste0("payload_ow_",rd_nb,".json"))
           command <- paste0("ibmcloud fn action invoke ",actionname," --blocking --param-file ",paste0("payload_ow_",rd_nb,".json"))
           check <- system(command)
           file.remove(paste0("payload_ow_",rd_nb,".json"))
         })
}
.faasr_user$operations$invoke_workflow <- faasr_invoke_workflow


faasr_set_workflow_timer <- function(cron, target=NULL){
  svc <- .faasr_get_svc()
  cred <- svc$cred
  faasr <- svc$json
  json_path <- svc$json_path
  cred <- faasr_collect_sys_env(faasr,cred)
  if (is.null(target)){
    target <- faasr$FunctionInvoke
  }
  type <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$FaaSType
  if (type == "GitHubActions"){
    faasr_set_workflow_timer_gh(faasr,target,cron)
  } else if (type == "Lambda"){
    faasr_set_workflow_timer_ld(faasr,target,cron)
  } else if (type == "OpenWhisk"){
    faasr_set_workflow_timer_ow(faasr,cred,target,cron)
  }
}
.faasr_user$operations$set_workflow_timer <- faasr_set_workflow_timer

faasr_unset_workflow_timer <- function(target=NULL){
  svc <- .faasr_get_svc()
  cred <- svc$cred
  faasr <- svc$json
  json_path <- svc$json_path
  cred <- faasr_collect_sys_env(faasr,cred)
  if (is.null(target)){
    target <- faasr$FunctionInvoke
  }
  type <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$FaaSType
  if (type == "GitHubActions"){
    faasr_set_workflow_timer_gh(faasr,target, cron=NULL, unset=TRUE)
  } else if (type == "Lambda"){
    faasr_set_workflow_timer_ld(faasr,target, cron=NULL, unset=TRUE)
  } else if (type == "OpenWhisk"){
    faasr_set_workflow_timer_ow(faasr,cred,target, cron=NULL, unset=TRUE)
  }
}
.faasr_user$operations$unset_workflow_timer <- faasr_unset_workflow_timer


faasr_set_workflow_timer_ld <- function(){
  
}

faasr_set_workflow_timer_ow <- function(faasr, cred, target, cron, unset=FALSE){
  
  endpoint <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$Endpoint
  faasr_w_cred <- FaaSr::faasr_replace_values(faasr, cred)
  faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
  faasr_json <- toString(faasr_json)
  faasr_json <- gsub("\"", "\\\\\\\"", faasr_json)
  if (unset==TRUE){
    if (endpoint == "" || grepl("cloud.ibm.com", endpoint)){
      command <- paste0("ibmcloud fn trigger delete ",target,"_trigger")
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot delete trigger")
        stop()
      }
      command <- paste0("ibmcloud fn rule delete ",target,"_rule")
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot delete rule")
        stop()
      }
    } else {
      command <- paste0("wsk trigger delete ",target,"_trigger")
      system(command)
      command <- paste0("wsk rule delete ",target,"_rule")
      system(command)
    }
  } else {
    if (endpoint == "" || grepl("cloud.ibm.com", endpoint)){
      command <- paste0("ibmcloud fn trigger create ",target,"_trigger --feed /whisk.system/alarms/alarm --param cron \"",cron,"\" --param trigger_payload \"",faasr_json,"\"")
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot create trigger")
        stop()
      }
      command <- paste0("ibmcloud fn rule create ",target,"_rule ",target,"_trigger ",target)
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot create rule")
        stop()
      }
    } else {
      command <- paste0("wsk trigger create ",target,"_trigger --feed /whisk.system/alarms/alarm --param cron \"",cron,"\" --param trigger_payload \"",faasr_json,"\"")
      system(command)
      command <- paste0("wsk rule create ",target,"_rule ",target,"_trigger ",target)
      system(command)
      command <- paste0("wsk trigger fire ",target,"_trigger")
      system(command)
    }
  }
}

faasr_set_workflow_timer_gh <- function(faasr, target, cron, unset=FALSE){
  
  folder <- faasr$FaaSrLog
  id <- faasr$InvocationID
  
  cat("[faasr_msg] Be cautious that mininum cron timer for github actions is 5minutes (*/5 * * * *), yours:", cron)
  if (!endsWith(target,".yml")){
    target_yml <- paste0(target,".yml")
  } else {
    target_yml <- target
  }
  
  workflow <- paste0(faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName,"/.github/workflows/",target_yml)
  if (!file.exists(workflow)){
    cat("\n\n[faasr_msg]Check that current working directory is correct and/or local repository exists\n\n")
    cat("\n\n[faasr_msg]Error: No workflow file found\n\n")
    stop()
  }
  
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
  
  writeLines(contents, workflow)
  wd <- getwd()
  setwd(faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName)
  user_name <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$UserName
  repo_name <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$ActionRepoName
  repo <- paste0(user_name,"/",repo_name)
  ref <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$Branch
  system("git init")
  msg <- paste0("git checkout -B ", ref)
  system(msg)
  system("git add .")
  system("git commit -m \'update repo\'")
  command <- paste0("git push -f http://github.com/", repo, " ", ref)
  check2 <- system(command)
  if (check2==0){
    cat("\n\n[faasr_msg] Successfully update the repo with cron timer\n")
  } else{
    cat("\n\n[faasr_msg] Error: Failed to update the repo with cron timer\n")
  }
  setwd(wd)
}


