#' @title Entry point to start execution of a FaaSr action
#' @description
#' This is the entry-point FaaSr Function that is invoked by a FaaS platform when an Action is instantiated
#' Terminology to clarify the various modules involved:
#'
#' * Action: an instance of a Docker container instantiated by a FaaS platform
#' * User Function: a single function written in R; at runtime, it is executed by a single Action
#' * FaaSr Function: function implemented by the FaaSr package to implement all the logic necessary to manage
#'     the execution of a User Function within an Action. A FaaSr function has a prefix faasr_
#' * User Workflow: a graph where each vertex represents a single User Functions and each edge represents a trigger
#' * Payload: a JSON-formatted text file that conforms to the FaaSr schema. It is delivered by the FaaS platform
#'     for each Action. It describes the entire User Workflow and may contain credentials for FaaS and S3 services
#'
#' faasr_start calls other FaaSr Functions to go through the following steps:
#'
#' * Parse the Payload and ensure that it conforms to the FaaSr JSON schema; otherwise, abort
#' * Build the User Workflow graph from Payload and ensure it is cycle-free; otherwise, abort
#' * Initialize the logs folder in an S3 bucket, only if this is the entry point to the User Workflow
#' * Ensure only a single User Function runs if it has multiple predecessors; otherwise, abort
#' * Invoke the User Function, supplying the parsed payload as a list argument
#' * Update the logs folder to assert that this User Function has completed
#' * Generate triggers to start Actions that will run the next User Functions, if there are any in the User Workflow
#' @param faasr_payload JSON Payload provided upon Action invocation by the FaaS platform

library("paws")

faasr_start <- function(faasr_payload) {

  # First, call faasr_parse to validate the Payload received upon Action invocation
  # If parsing is successful, faasr is an R list storing the parsed JSON Payload
  # If parsing is not successful, the Action aborts with stop() before executing the User Function
  .faasr <<- faasr_parse(faasr_payload)

  # Build a data structure representing the User Workflow, from the parsed Payload
  # FaaSr only supports Directed Acyclic Graph (DAG) User Workflows, with a single entry point
  # If the User Workflow is not a DAG, the Action aborts with stop() before executing the User Function
  # Returning value is predecessor list. It uses "faasr_predecessors_list() function inside of it
  # Make a list of all User Functions that are the predecessors of this User Function, if any
  pre <- faasr_check_workflow_cycle(.faasr)

  # Check endpoint/region of the Logging server / TBD: Check all the DataStores servers
  # If endpoint is not defined(length==0) or empty(=="", for else cases), set empty value ""
  # If endpoint is not empty(not S3), check whether endpoint starts with http
  # If region is not defined(length==0) or empty(==""), set any value("region" in this case).
  .faasr <<- faasr_s3_check(.faasr)

  # If this User Function has zero predecessors, it is the single entry point
  # In this case, create the log folder in S3 logs buket
  # All logs, as well as any synchronization among actions, will be stored in the log folder
  # The log folder must be a unique name for each Action, such as a user-provided unique timestamp or UUID
  # If a log folder already exists, the Action aborts with stop() before executing the User Function
  if (length(pre) == 0) {
    .faasr <<- faasr_init_log_folder(.faasr)
  }

  # If a User Function has more than one predecessors, this Action may or may not invoke the User Function
  # Only the last Action should invoke the User Function; all other Action invocations must abort
  if (length(pre) > 1) {
    faasr_abort_on_multiple_invocations(.faasr, pre)
  }
  
  # If the Action reaches this point without aborting, it is ready to invoke the User Function
  # Extract the name of the User Function from the Payload, and invoke it, passing the parsed Payload as arg
  # try get(faasr$FunctionInvoke) and if there's an error, return error message and stop the function
  user_function = tryCatch(expr=get(.faasr$FunctionInvoke), error=function(e){
    err_msg <- paste0('{\"faasr_start\":\"Cannot find FunctionInvoke ',.faasr$FunctionInvoke,', check the name and sources\"}', "\n")
    cat(err_msg)
    result <- faasr_log(err_msg)
    stop()
    }
  )
  
  # Get a list of the current function's arguments.
  user_args = faasr_get_user_function_args(.faasr)
  
  # Use do.call to use user_function with arguments
  # try do.call and if there's an error, return error message and stop the function
  faasr_result <- tryCatch(expr=do.call(user_function, user_args), error=function(e){
    nat_err_msg <- paste0('\"faasr_start\": ', as.character(e))
    err_msg <- paste0('{\"faasr_start\":\"Errors in the user function: ',.faasr$FunctionInvoke,', check the log for the detail \"}', "\n")
    cat(nat_err_msg)
    cat(err_msg)
    result <- faasr_log(err_msg)
    result_2 <- faasr_log(nat_err_msg)
    stop()
    }
  )

  # At this point, the Action has finished the invocation of the User Function
  # We flag this by uploading a file with name FunctionInvoke.done with contents TRUE to the S3 logs folder
  # Check if directory already exists. If not, create one
  log_folder <- paste0(.faasr$FaaSrLog,"/",.faasr$InvocationID)
  if (!dir.exists(log_folder)) {
    dir.create(log_folder, recursive=TRUE)
  }
  file_name <- paste0(.faasr$FunctionInvoke, ".done")
  write.table("TRUE", file=paste0(log_folder, "/", file_name), row.names=F, col.names=F)
  faasr_put_file(.faasr$LoggingServer, log_folder, file_name, log_folder, file_name)

  # Now trigger the next Actions(s), if there are any in the User Workflow
  faasr_trigger(.faasr)

  # Log to standard output
  msg_1 <- paste0('{\"faasr_start\":\"Finished execution of User Function ',.faasr$FunctionInvoke,'\"}', "\n")
  cat(msg_1)
  result <- faasr_log(msg_1)
  msg_2 <- paste0('{\"faasr_start\":\"With Action Invocation ID is ',.faasr$InvocationID,'\"}', "\n")
  cat(msg_2)
  result <- faasr_log(msg_2)
}
