test_that("get_user_function_args", {
  ## faasr setup is required
  ## Argument is a list.
  ## faasr_get_user_function_args() is expected to return an arguments list in the FunctionInvoke function
  ## In this example, the function should return "F1"'s Arguments list, not "F2" or else.
  faasr <- list()
  faasr$FunctionInvoke <- "F1"
  faasr$FunctionList$F1$Arguments <- list("f1_arg1"="arg1.txt")
  faasr$FunctionList$F2$Arguments <- list("f2_arg1"="arg2.txt")
  
  expect_equal(faasr_get_user_function_args(faasr), list("f1_arg1"="arg1.txt"))
})
