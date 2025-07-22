#' @name faasr_get_resource_requirements
#' @title faasr_get_resource_requirements
#' @description
#' Extract resource requirements for a function with fallback hierarchy:
#' Function-level → Server-level → Default values
#' @param faasr a list form of the JSON file
#' @param actionname name of the action/function
#' @param server_info server configuration information
#' @return list of resource requirements
#' @keywords internal

faasr_get_resource_requirements <- function(faasr, actionname, server_info) {
  # Get function-level resources if available
  function_resources <- faasr$FunctionList[[actionname]]$Resources
  
  # Apply fallback hierarchy: Function → Server → Default
  list(
    partition = function_resources$Partition %||% server_info$Partition %||% "faasr",
    nodes = as.integer(function_resources$Nodes %||% server_info$Nodes %||% 1),
    tasks = as.integer(function_resources$Tasks %||% server_info$Tasks %||% 1),
    cpus_per_task = as.integer(function_resources$CPUsPerTask %||% server_info$CPUsPerTask %||% 1),
    memory_mb = as.integer(function_resources$Memory %||% server_info$Memory %||% 1024),
    time_limit = as.integer(function_resources$TimeLimit %||% server_info$TimeLimit %||% 60),
    working_dir = function_resources$WorkingDirectory %||% server_info$WorkingDirectory %||% "/tmp"
  )
}