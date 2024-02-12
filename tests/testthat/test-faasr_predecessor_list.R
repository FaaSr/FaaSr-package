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
  
  # draw a result. In this first example, the function should return precedessors graph
  result <- list()
  result$F2 <- "F1"
  result$F3 <- "F2"
  
  ## check the result
  expect_equal(faasr_predecessors_list(faasr, graph), result)
  
  #############################################################
  #############################################################
  ## Another more complex example
  faasr <- list()
  faasr$FunctionInvoke <- "F1"
  faasr$FunctionList$F1 <- list()
  faasr$FunctionList$F2 <- list()
  faasr$FunctionList$F3 <- list()
  faasr$FunctionList$F4 <- list()
  faasr$FunctionList$F5 <- list()
  faasr$FunctionList$F6 <- list()

  graph <- list()
  graph$F1 <- c("F2", "F3")
  graph$F2 <- c("F4", "F5")
  graph$F3 <- "F4"
  graph$F4 <- c("F5", "F6")  
  
  ## This time, result should be following 
  result <- list()
  result$F2 <- "F1"
  result$F3 <- "F1"
  result$F4 <- c("F2", "F3")
  result$F5 <- c("F2", "F4")
  result$F6 <- "F4"
  
  ## check the result
  expect_equal(faasr_predecessors_list(faasr, graph), result)
  
  ## Make a complex graph and faasr with different functions. Change the faasr$FunctionInvoke
  ## and check the result.
})
