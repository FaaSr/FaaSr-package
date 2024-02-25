# define a list for storing functions
.faasr_user <- list()

# defined variables
faasr_gh_local_repo <- "faasr_gh_local_repo"
faasr_data <- "faasr_data"
basic_gh_image <- "ghcr.io/faasr/github-actions-tidyverse:latest"
basic_ow_image <- "faasr/openwhisk-tidyverse:latest"
basic_ld_image_account <- "145342739029.dkr.ecr."
basic_ld_image_account_id <- "145342739029"
basic_ld_image_tag <- ".amazonaws.com/aws-lambda-tidyverse:latest"

#' @name faasr_register_workflow
#' @title faasr_register_workflow
#' @description 
#' Client tools for registering actions
#' This aggregates openwhisk, github actions and lambda's
#' register functions
#' @param ... inputs for timeout, cron, and memory
#' @import cli
#' @import httr
#' @return return nothing / executes the FaaS
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr("test.json", "env")
#' test$register_workflow
#' }

# faasr_register_workflow function
faasr_register_workflow <- function(...){

  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  on.exit(setwd(faasr_wd))
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  
  setwd(faasr_wd)

  # register actions for openwhisk/github-actions/lambda by given json
  check <- faasr_register_workflow_openwhisk(faasr,cred,...)
  check <- faasr_register_workflow_github_actions(faasr,cred)
  check <- faasr_register_workflow_aws_lambda(faasr,cred,...)
  
}
.faasr_user$operations$register_workflow <- faasr_register_workflow


#' @title faasr_collect_sys_env
#' @description 
#' Collect system environment from the R studio
#' If the credential name is property set (Servername + Key name),
#' this function will use Sys.getenv to collect credentials.
#' If there's already credentials in the list, it skips the procedure.
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @return credential list
#' @import cli
#' @keywords internal

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
          err_msg <- paste0("faasr_collect_sys_env: ",cred_name," requires values")
          cli_alert_warning(err_msg)
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
          err_msg <- paste0("faasr_collect_sys_env: ",cred_name," requires values")
          cli_alert_warning(err_msg)
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
          err_msg <- paste0("faasr_collect_sys_env: ",cred_name_ac," requires values")
          cli_alert_warning(err_msg)
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
          err_msg <- paste0("faasr_collect_sys_env: ",cred_name_sc," requires values")
          cli_alert_warning(err_msg)
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
        err_msg <- paste0("faasr_collect_sys_env: ",cred_name_ac," requires values")
        cli_alert_warning(err_msg)
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
        err_msg <- paste0("faasr_collect_sys_env: ",cred_name_sc," requires values")
        cli_alert_warning(err_msg)
      } else{
        cred[[cred_name_sc]] <- real_cred
      }
    }
  }
  return(cred)
}

#' @title .faasr_get_svc 
#' @description 
#' Helper function to get the saved data for 
#' json, cred, and operations.
#' @return object containing all the information
#' @keywords internal

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


#' @title faasr
#' @description 
#' FaaSr library client-side main function
#' It's generating the instance for the user
#' Users can use the functions with the instance generated by "faasr"
#' @param json_path a string for the json path
#' @param env_path a string for the env(credentials) path
#' @return svc; a set of data consisting of functions and data
#' @import cli
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr(json_path="json_path.json", env_path="env_path")
#' }

# faasr main function
faasr <- function(json_path=NULL, env_path=NULL){

  cli::cli_h1(paste0("Start FaaSr client tools"))

  if (json_path=="" || is.null(json_path)){
    cli_alert_danger("faasr: JSON path is required")
    return("")
  } else if (!file.exists(json_path)){
    cli_alert_danger("faasr: JSON path is invalid")
    return("")
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
    
    if (!is.null(svc$json$DataStores[[data_js]]$SecretKey)){
      if (svc$json$DataStores[[data_js]]$SecretKey != paste0(data_js,"_SECRET_KEY")){
        svc$cred[[paste0(data_js,"_SECRET_KEY")]] <- svc$json$DataStores[[data_js]]$SecretKey
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
  succ_msg <- paste0("Successfully get configuration from ", json_path, " and ", env_path)
  cli_alert_success(succ_msg)

  # create dir
  if (!dir.exists(faasr_gh_local_repo)){
    dir.create(faasr_gh_local_repo)
    succ_msg <- paste0("Create the FaaSr directory: ", faasr_gh_local_repo)
    cli_alert_success(succ_msg)
  }
  if (!dir.exists(faasr_data)){
    dir.create(faasr_data)
    succ_msg <- paste0("Create the FaaSr directory: ", faasr_data)
    cli_alert_success(succ_msg)
  }

  cli_alert_success("Ready to use FaaSr client tools:")
  cli_ol()
  ulid <- cli_ul()
  cli_li("$register_workflow")
  cli_li("$invoke_workflow")
  cli_li("$set_workflow_timer")
  cli_li("$unset_workflow_timer")
  cli_end(ulid)
  cli_end()

  return(svc)
}


#' @title faasr_replace_values
#' @description 
#' replace dummy keys with real keys
#' Dummy key format: Servername + "_" + Key Name with CAPITAL LETTER
#' e.g., My_OW_Account + API.key = My_OW_Account_API_KEY
#' @param faasr a list form of the JSON file
#' @param cred a list form of the credentials
#' @return faasr a list form of the JSON file with real keys
#' @import cli
#' @export
#' @examples
#' if (interactive()){
#' faasr_with_cred <- faasr_replace_values(faasr, cred)
#' }

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


#' @title faasr_invoke_workflow
#' @description 
#' invoke workflow 
#' This function aggregates the invoke workflow function for
#' openwhisk, github actions and lambda
#' This can be used as a cross-platform function
#' @param FunctionInvoke a string for invoke function
#' @param ... a string for underlying functions
#' @return return nothing / invokes the FaaS platform.
#' @import cli
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr("test.json", "env")
#' test$invoke_workflow
#' }

# invoke first action
faasr_invoke_workflow <- function(FunctionInvoke=NULL, ...){

  # get the "svc" by using "faasr_get_svc" and define the required variables.
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  on.exit(setwd(faasr_wd))
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
           faasr_workflow_invoke_lambda(faasr, cred, faas_name, actionname)
         },
         # If first action is openwhisk, use ibmcloud
         "OpenWhisk"={
           faasr_workflow_invoke_openwhisk(faasr, cred, faas_name, actionname, ...)
         })
}
.faasr_user$operations$invoke_workflow <- faasr_invoke_workflow


#' @title faasr_set_workflow_timer
#' @description 
#' set cron timer for workflow 
#' This function aggregates the set cron timer function for
#' openwhisk, github actions and lambda
#' This can be used as a cross-platform function
#' @param cron a string for cron data e.g., */5 * * * *
#' @param target a string for specific function
#' @param ... a string for underlying functions
#' @return return nothing / set the workflow timer
#' @import cli
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr("test.json", "env")
#' test$set_workflow_timer
#' }


# set the cron timer
faasr_set_workflow_timer <- function(cron, target=NULL, ...){

  # get env
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  on.exit(faasr_wd)
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  if (is.null(target)){
    target <- faasr$FunctionInvoke
  }

  setwd(faasr_wd)

  if (is.null(cron) || cron==""){
    cli_alert_danger("faasr_set_workflow_timer: No cron time provided")
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


#' @title faasr_unset_workflow_timer
#' @description 
#' unset cron timer for workflow 
#' This function aggregates the unset cron timer function for
#' openwhisk, github actions and lambda
#' This can be used as a cross-platform function
#' @param target a string for specific function
#' @param ... a string for underlying functions
#' @return return nothing / unset the workflow timer
#' @import cli
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr("test.json", "env")
#' test$unset_workflow_timer
#' }

# unset the timer
faasr_unset_workflow_timer <- function(target=NULL,...){

  # get env
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  on.exit(setwd(faasr_wd))
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

