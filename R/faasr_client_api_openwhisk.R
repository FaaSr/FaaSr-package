#' @title Client tool for registering openwhisk actions.
#' @description faasr_register_workflow_ibmcloud_openwhisk: 
#' starts to register functions in the JSON file for OpenWhisk
#' @description faasr_register_workflow_ibmcloud_action_lists: 
#' creates a list of Server and Action name pairs               
#' @description faasr_register_workflow_ibmcloud_target_group: 
#' targets a group: ibmcloud openwhisk needs to target group/default is "Default"             
#' @description faasr_register_workflow_ibmcloud_create_namespace: 
#' creates a new namespace if given namespace doesn't exist  
#' @description faasr_register_workflow_ibmcloud_target_namespace: 
#' targets a namespace with the given namespace              
#' @description faasr_register_workflow_ibmcloud_create_action:
#' registers the functions with ibmcloud API              
#'
#' @param faasr_register_workflow_ibmcloud_openwhisk: 
#' "faasr" for list of the json, "cred" for the list of credentials       
#' @param faasr_register_workflow_ibmcloud_action_lists:
#' "faasr" for list of the json
#' @param faasr_register_workflow_ibmcloud_create_namespace:
#' "faasr" for list of the json 
#' @param faasr_register_workflow_ibmcloud_target_namespace: 
#' "name" for the string
#' @param faasr_register_workflow_ibmcloud_create_action:
#' "actionname" for the string, "faasr" for list of the json 
#' 
#' @return faasr_register_workflow_ibmcloud_openwhisk: 
#' "faasr" for list of the json
#' @return faasr_register_workflow_ibmcloud_action_lists: 
#' "action_list" for the list of servername:actionname pairs
#' @return faasr_register_workflow_ibmcloud_create_namespace:
#' "name_id" for the string of namespace id

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
      cat("\n\n[faasr_msg]invalid server:", server_name," check server type\n\n")
      stop()
    }
    if (faasr$ComputeServers[[server_name]]$FaaSType == "OpenWhisk") {
      action_name <- fn
      action_list[[server_name]] <- unique(c(action_list[[server_name]],action_name))
    }
  }
  return(action_list)
}

faasr_register_workflow_ibmcloud_target_group <- function(){
  command <- paste0()
  cat("[faasr_msg]Target Resource group(Type \"Enter\" to proceed with default value): ")
  check <- readline()
  if (check==""){
    command <- paste0("ibmcloud target -g Default")
  } else{
    command <- paste0("ibmcloud target -g ",check)
  }
  
  response <- system(command, ignore.stdout=TRUE, ignore.stderr=TRUE)
  
  if (response==0){
    cat("\n\n[faasr_msg]Target Resource group Success\n\n")
  } else{
    cat("\n\n[faasr_msg]Target Resource group Failed, please check resource group\n\n")
    stop()
  }
}

# create a namespace
faasr_register_workflow_ibmcloud_create_namespace <- function(name) {
  command <- paste0("ibmcloud fn namespace create ",name)
  cat("[faasr_msg]creating a new namespace\n")
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
  cat("\n\n[faasr_msg][faasr_msg]targetting a namespace:",namespace,"\n\n")
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
    cat("\n\n[faasr_msg]Invalid Namespace\n")
    cat("[faasr_msg]Create a new Namespace?[y/n]\n")
    while(TRUE) {
      check <- readline()
      if (check=="y") {
        cat("\n\n[faasr_msg]type Namespace name: ")
        # receive the user's input for the namespace name
        name <- readline()
        # create a new namespace
        namespace <- faasr_register_workflow_ibmcloud_create_namespace(name)
        # save the result to the json file
        faasr$ComputeServers[[server]]$Namespace <- namespace
        cat("\n\n[faasr_msg]creating namespace successful", "\n")
        cat("\n\n[faasr_msg]New Namespace name is: ",namespace)
        break
      } else if(check=="n") {
        cat("\n\n[faasr_msg]stop the function\n")
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
#faasr_register_workflow_ibmcloud_login_ibm <- function(server,cred) {
  # retrieve api key and try login by using it
  #api_key <- cred[[paste0(server,"_API_KEY")]]
  #command <- paste("ibmcloud login --apikey",api_key)
  #check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE, input="y")
  # if check == 0, i.e., no errors, return TRUE
  #if (check==0) {
    #cat("\n\n[faasr/msg] Login Success\n\n")
    #return(TRUE)
    # if check != 0, i.e., errors, ask the user to create a new one
  #} else {
    #cat("\n\nInvalid API key\n\n")
    #stop()
  #}
#}


# create an action
faasr_register_workflow_ibmcloud_create_action <- function(actionname, faasr) {
  # actioncontainer can be either default or user-customized
  if (length(faasr$ActionContainers[[actionname]])==0 || faasr$ActionContainers[[actionname]] == "") {
    actioncontainer <- "faasr/openwhisk-tidyverse"
  } else {
    actioncontainer <- faasr$ActionContainers[[actionname]]
  }
  # create a function with maximum timeout and 512MB memory space
  command <- paste("ibmcloud fn action create",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 2048")
  cat("\n\n[faasr_msg]creating a new action\n")
  check <- system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
  # if action already exists, ask the user to update the action
  if (check[1]==153) {
    cat("[faasr_msg]Error: action name:\"",actionname,"\"already exists\n")
    cat("[faasr_msg]Do you want to update the action?[y/n]\n")
    while(TRUE) {
      check <- readline()
      if (check=="y") {
        # update the action
        command <- paste("ibmcloud fn action update",actionname,"--docker",actioncontainer,"--timeout 600000 --memory 2048")
        cat("\n[faasr_msg]updating an action\n")
        system(command,ignore.stdout=TRUE, ignore.stderr=TRUE)
        cat("[faasr_msg]successful", "\n")
        break
      } else if(check=="n") {
        stop()
      } else {
        cat("Enter \"y\" or \"n\": ")
      }
    }
  }
}

# set workflow timer for openwhisk
faasr_set_workflow_timer_ow <- function(faasr, cred, target, cron, unset=FALSE){

  # set variables
  endpoint <- faasr$ComputeServers[[faasr$FunctionList[[target]]$FaaSServer]]$Endpoint
  faasr_w_cred <- FaaSr::faasr_replace_values(faasr, cred)
  faasr_json <- jsonlite::toJSON(faasr_w_cred, auto_unbox=TRUE)
  faasr_json <- toString(faasr_json)
  # json file should get out two layers, so escaping letter should be twice
  faasr_json <- gsub("\"", "\\\\\\\"", faasr_json)
  # if unset==TRUE, delete the rule and trigger
  if (unset==TRUE){
    # if endpoint is empty or contains "cloud.ibm.com", use "ibmcloud fn" cli
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
    # if not, use "wsk" cli
    } else {
      command <- paste0("wsk trigger delete ",target,"_trigger")
      system(command)
      command <- paste0("wsk rule delete ",target,"_rule")
      system(command)
    }
  # if unset=FALSE, set the rule and trigger
  } else {
    # if endpoint is empty or contains "cloud.ibm.com", use "ibmcloud fn" cli
    if (endpoint == "" || grepl("cloud.ibm.com", endpoint)){
      # create the trigger with cron and faasr json payload
      command <- paste0("ibmcloud fn trigger create ",target,"_trigger --feed /whisk.system/alarms/alarm --param cron \"",cron,"\" --param trigger_payload \"",faasr_json,"\"")
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot create trigger")
        stop()
      }
      # create the rule, rule links the trigger and the function
      command <- paste0("ibmcloud fn rule create ",target,"_rule ",target,"_trigger ",target)
      result <- system(command)
      if (result!=0){
        cat("[faasr_msg] Error: cannot create rule")
        stop()
      }
    # if not, use the "wsk" cli
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

