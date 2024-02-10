#' @name faasr_predecessors_list
#' @title faasr_predecessors_list
#' @description 
#' Get the predecessors list, by using the graph from faasr_check_workflow
#' @param faasr list with parsed and validated Payload
#' @param graph graph constructed by the depth first search in faasr_check_workflow_cycle
#' @return a list of "function:predecessors" pairs.
#' @export

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
