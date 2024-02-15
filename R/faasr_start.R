#' @name faasr_start
#' @title faasr_start
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
#' @return faasr a list form of JSON payload 
#' @importFrom "utils" "write.table"
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' faasr <- faasr_start(faasr_payload)
#' }

globalVariables(".faasr")

faasr_start <- function(faasr_payload) {

  # First, call faasr_parse to validate the Payload received upon Action invocation
  # If parsing is successful, faasr is an R list storing the parsed JSON Payload
  # If parsing is not successful, the Action aborts with stop() before executing the User Function
  .faasr <- faasr_parse(faasr_payload)

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
  .faasr <- faasr_s3_check(.faasr)

  # If this User Function has zero predecessors, it is the single entry point
  # In this case, create the log folder in S3 logs buket
  # All logs, as well as any synchronization among actions, will be stored in the log folder
  # The log folder must be a unique name for each Action, such as a user-provided unique timestamp or UUID
  # If a log folder already exists, the Action aborts with stop() before executing the User Function
  if (length(pre) == 0) {
    .faasr <- faasr_init_log_folder(.faasr)
  }

  # If a User Function has more than one predecessors, this Action may or may not invoke the User Function
  # Only the last Action should invoke the User Function; all other Action invocations must abort
  if (length(pre) > 1) {
    faasr_abort_on_multiple_invocations(.faasr, pre)
  }

  return(.faasr)
}
