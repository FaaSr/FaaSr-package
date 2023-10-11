#' @title Check if the User Function has any predecessors in the Workflow
#' @description TBD
#' @param faasr list with parsed and validated Payload
#' @param graph graph constructed by the depth first search in faasr_check_workflow_cycle

faasr_predecessors_list <- function(faasr, graph){
  # find the predecessors and add them to the list "pre"
  pre <- list()
  for (func1 in names(graph)){
    for (func2 in graph[[func1]]){
      pre[[func2]] <- c(pre[[func2]], func1)
    }
  }
  return(pre)
}
