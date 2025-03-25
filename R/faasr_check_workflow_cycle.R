#' @name faasr_check_workflow_cycle
#' @title faasr_check_workflow_cycle
#' @description 
#' Uses graph depth-first search algorithm to detect cycles
#' @param faasr list with parsed and validated Payload
#' @return graph a graph representation of the Workflow
#' @keywords internal

# workflow implementation - check loop iteratively, predecessors.
# check workflows for cycle. Returns predecessor list
faasr_check_workflow_cycle <- function(faasr){

  # implement dfs cycle detection - recursive function (returns boolean)
  is_cyclic <- function(node){
    # check that current action is in function list
    if (!(node %in% names(faasr$FunctionList))){
      err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"invalid function trigger: ',node,'\"}', "\n")
      message(err_msg)
      stop()
      }

    # if the current action is already in the recursive call stack
    # then there must be a cycle
    if (isTRUE(stack[[node]])) {
      return(TRUE)
	  }

	  # mark action as visited and add it to recursive call stack
	  stack[[node]] <<- TRUE
    visited[[node]] <<- TRUE

	  # recursively check all of the
    # action's successors
    if(!is.null(adj_graph[[node]])){
      for (action in adj_graph[[node]]) {
        # if the successor action's branch creates a cycle
        # then the graph is cyclical
        if (!(isTRUE(visited[[action]])) && is_cyclic(action)) {
	        err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"cycle created by ', node, ' invoking ', action, '\"}', "\n")
	        message(err_msg)
	        stop()
        # if the successor is in the recursion call stack, then
        # there must be a  cycle
        } else if (isTRUE(stack[[action]])) {
	        err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"cycle detected in graph create by ', node, ' invoking ', action, '\"}', "\n")
	        message(err_msg)
	        stop()
        }
      }
    }
    # Finished exploring branch. Remove node from recursion stack
    stack[[node]] <<- FALSE
    return(FALSE)
  }
	
  # build empty lists for the graph
  adj_graph <- list()

  # build the graph indicating adjacent nodes, e.g., "F1":["F2","F3"], so on.
  for (func in names(faasr$FunctionList)) {
    if (length(faasr$FunctionList[[func]]$InvokeNext) != 0){
      for (invoke_next in faasr$FunctionList[[func]]$InvokeNext){
        parts <- unlist(strsplit(invoke_next, "[()]"))
        adj_graph[[func]] <- unique(c(adj_graph[[func]], parts[1]))
      }
    }
  }

  # call faasr_predecessors_list and get a list of function:predecessor pairs
  pre <- faasr_predecessors_list(faasr, adj_graph)

  # finds first function in the graph
  start <- FALSE
  for(func in names(faasr$FunctionList)){
    if(is.null(pre[[func]])){
      start <- TRUE
      first_func <- func
    }
  }

  # if there is no functions with no predecessors, then there is a cycle
  if(start == FALSE){
    err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"function loop found: no initial node\"}', "\n")
    message(err_msg)
    stop()
  }

  # build an empty recursion call stack
  stack <- list()
  # build an empty visited list
  visited <- list()
  # do dfs cycle detection starting with first action
  cycle <- is_cyclic(first_func)
	
  # check for unreachable functions
  check <- TRUE
  for (func in names(faasr$FunctionList)){
    if(!(func %in% names(visited))){
      err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"unreachable action: ',func,'\"}', "\n")
      message(err_msg)
      stop()
    }
  }
	# return predecessor list for current action
  return(pre[[faasr$FunctionInvoke]])
}
