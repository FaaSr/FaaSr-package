#' @title Check if the Workflow has any cycles
#' @description Uses graph depth-first search algorithm to detect cycles
#' @param faasr list with parsed and validated Payload
#' @return graph a graph representation of the Workflow

# workflow implementation - check loop iteratively, predecessors.
faasr_check_workflow_cycle <- function(faasr){
  
  # implement dfs - recursive function
  dfs <- function(start, target){

    # find target in the graph's successor. If it matches, there's a loop
    if (target %in% graph[[start]]) {
	  err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"function loop found in ',target,'\"}', "\n")
	  cat(err_msg)
	  faasr_log(err_msg)
	  stop()
	}

	# add start, marking as "visited"
	stack <<- c(stack, start)

	# set one of the successors as another "start"
	for (func in graph[[start]]) {

	  # if new "start" has been visited, do nothing
	  if (func %in% stack) {
	    NULL
	  # if not, keep checking the DAG.
	  } else {
	    dfs(func, target)
	  }
	}
  }
	
  # build empty lists for the graph and predecessors.
  graph <- list()

  # build the graph indicating adjacent nodes, e.g., "F1":["F2","F3"], so on.
  for (func in names(faasr$FunctionList)) {
    graph[[func]] <- faasr$FunctionList[[func]]$InvokeNext
  }

  # check next functions of FunctionInvoke are in the function list
  for (func in names(graph)){
    for (path in graph[[func]]){
      if (!(path %in% names(faasr$FunctionList))){
        err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"invalid next function ',path,' is found in ',func,'\"}', "\n")
        cat(err_msg)
        faasr_log(err_msg)
        stop()
      }
    }
  }

  # call faasr_predecessors_list and get a list of function:predecessor sets.
  pre <- faasr_predecessors_list(faasr, graph)

  # find the initial function(pre==0)
  for (func in names(faasr$FunctionList)){
    if (is.null(pre[[func]])){
      # build an empty list of stacks - this will prevent the infinite loop
      stack <- list()
      # if it is the initial function, do dfs starting with it.
      dfs(faasr$FunctionInvoke, faasr$FunctionInvoke)

      # check unreachable states by comparing function list and stack
      for (func in names(faasr$FunctionList)){
        if (!(func %in% stack)){
          err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"unreachable state is found in ',func,'\"}', "\n")
          cat(err_msg)
          faasr_log(err_msg)
          stop()
        }
      }
    }
  }
  return(pre[[faasr$FunctionInvoke]])
}
