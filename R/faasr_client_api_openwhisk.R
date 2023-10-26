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
      cat("\n\ninvalid server:", server_name," check server type\n\n")
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
  if (length(faasr$ActionContainers[[actionname]])==0) {
    actioncontainer <- "faasr/openwhisk-tidyverse"
  } else {
    actioncontainer <- faasr$ActionContainers[[factionname]]
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

