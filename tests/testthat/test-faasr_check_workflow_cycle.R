test_that("check workflow cycle", {
  
  ## Create "faasr" list
  ## Make a wrong case: Loop, Unreachable state and so on...
  ## "faasr$FunctionInvoke" is the starting node.
  ## You can make a "->" by setting "faasr$FunctionList$[departure]$InvokeNext <- [destination]"
  ## For the example below, it can be shown that "F1" -> "F2", "F2" -> "F1".
  ## Make different/various error cases. 
  ## expect_error(faasr_check_workflow_cycle(faasr)) is to expect that this creates error message.
  faasr <- list()
  faasr$FunctionInvoke <- "F1"
  faasr$FunctionList$F1$InvokeNext <- "F2"
  faasr$FunctionList$F2$InvokeNext <- "F1"
  
  expect_error(faasr_check_workflow_cycle(faasr))
  
  #######################################################
  #######################################################
  
  ## Error case: Invalid name found
  ## "faasr$FunctionList$[function name] <- list()" defines a node without pointing next node.
  ## In this example, "F1" -> "F2", "F1" -> "F3", "F3" -> "F4". It doesn't seem to have a problem.
  ## But "F4" is not in the "FunctionList" as it is not defined as "faasr$FunctionList$F4 <- list()"
  faasr$FunctionList$F1$InvokeNext <- c("F2","F3")
  faasr$FunctionList$F2 <- list()
  faasr$FunctionList$F3$InvokeNext <- "F4"
  
  expect_error(faasr_check_workflow_cycle(faasr))
  
  #######################################################
  #######################################################
  
  # Error case: Unreachable state
  ## In this example, there are 3 nodes, "F1","F2" and "F3". And "F1" is pointing "F2": F1" -> "F2"
  ## You can find that there are no way to reach "F3", because no node is pointing it.
  faasr$FunctionList$F1$InvokeNext <- "F2"
  faasr$FunctionList$F2 <- list()
  faasr$FunctionList$F3 <- list()
  
  expect_error(faasr_check_workflow_cycle(faasr))
  
  #######################################################
  #######################################################
  
  ## Now set up expected correct response.
  ## faasr should be defined as follows. In this case, there should be no errors.
  faasr$FunctionList$F1$InvokeNext <- c("F2","F3")
  faasr$FunctionList$F2$InvokeNext <- "F4"
  faasr$FunctionList$F3$InvokeNext <- "F4"
  faasr$FunctionList$F4 <- list()
  
  ## result should be defined to compare it.
  ## result is the predecessors of the given FunctionInvoke
  result <- list()
  result$F1 <- c("F2","F3")
  result$F2 <- "F4"
  result$F3 <- "F4"
  pre <- faasr_predecessors_list(faasr, result)
  result <- pre[[faasr$FunctionInvoke]]

  # Now use "expect_equal". 
  expect_equal(faasr_check_workflow_cycle(faasr), result)
  
  #######################################################
  #######################################################
})
