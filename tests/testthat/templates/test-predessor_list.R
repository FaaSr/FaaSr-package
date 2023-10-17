test_that("predecessor_list", {
  ## this function requires two values: faasr, graph
  ## this function returns a character array "pre", which are predecessors.
  ## It is expected to get an empty list if the FunctionInvoke is set to first action.
  faasr <- list()
  faasr$FunctionInvoke <- "F1"
  faasr$FunctionList$F1 <- list()
  faasr$FunctionList$F2 <- list()
  faasr$FunctionList$F3 <- list()
  ## graph is the result of function "check_workflow_cycle". You can make it either by using it or
  ## by yourself. In this example, graph is written by hands.
  graph <- list()
  graph$F1 <- "F2"
  graph$F2 <- "F3"
  
  # draw a result. In this first example, the function should return "empty list" because
  # faasr$FunctionInvoke is "F1" and "F1" has no predecessor nodes.
  result <- list()
  
  ## check the result
  expect_equal(faasr_predecessors_list(faasr, graph), result)
  ## Check the length as well
  expect_length(faasr_predecessors_list(faasr,graph), 0)
  
  #############################################################
  #############################################################
  ## Now change the faasr$FunctionInvoke.
  faasr$FunctionInvoke <- "F2"
  
  ## This time, result should return "F1" 
  ## result should be "list", even if it has only one character values.
  result <- list("F1")
  
  ## check the result
  expect_equal(faasr_predecessors_list(faasr, graph), result)
  ## Check the length as well
  expect_length(faasr_predecessors_list(faasr,graph), 1)
  
  ## Make a complex graph and faasr with different functions. Change the faasr$FunctionInvoke
  ## and check the result.
})
