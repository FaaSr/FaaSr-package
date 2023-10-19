test_that("register_github_repository_list", {
  ## This function extracts a list of [server name: actionname] pairs
  ## input is the list "faasr"
  ## "faasr" requires "FunctionList" and "ComputeServers"
  ## In "FunctionList", "FaaSServer" and "Actionname" should be defined.
  faasr <- list()
  faasr$FunctionList$F1$FaaSServer <- "Github_Account_1"
  faasr$FunctionList$F2$FaaSServer <- "Github_Account_2"
  faasr$FunctionList$F3$FaaSServer <- "Github_Account_3"
  
  faasr$FunctionList$F1$Actionname <- "F1_action"
  faasr$FunctionList$F2$Actionname <- "F2_action"
  faasr$FunctionList$F3$Actionname <- "F3_action"
  
  ## In "ComputeServers", "FaaSType" should be defined with the name matched.
  faasr$ComputeServers$Github_Account_1$FaaSType <- "GitHubActions"
  faasr$ComputeServers$Github_Account_2$FaaSType <- "GitHubActions"
  faasr$ComputeServers$Github_Account_3$FaaSType <- "GitHubActions"
  
  ## Execute the function and save the result to the list "result"
  result <- faasr_register_workflow_github_repo_lists(faasr)
  
  ## Check the result by picking up some values
  ## Key is server name, Value is Actionname
  expect_equal(result$Github_Account_1, "F1_action")
  expect_equal(result$Github_Account_2, "F2_action")
  
  #############################################################################
  #############################################################################
  ## We can also assume that "Github_Account_2"'s FaaSType is accidentally empty NULL
  faasr$ComputeServers$Github_Account_2$FaaSType <- NULL

  ## Execute the function.
  ## We can expect it'll return the error
  expect_error(faasr_register_workflow_github_repo_lists(faasr))
  
  
  faasr$ComputeServers$Github_Account_2$FaaSType <- "GitHubActions"
  
  #############################################################################
  #############################################################################
  ## We can also assume that "Github_Account_3"'s FaaSType is Lambda
  faasr$ComputeServers$Github_Account_3$FaaSType <- "Lambda"
  
  ## Execute the function and save the result to the list "result"
  result <- faasr_register_workflow_github_repo_lists(faasr)
  
  ## Check the result by picking up some values
  ## We can expect that "Github_Account_3" will return nothing because it is "Lambda"
  expect_equal(result$Github_Account_3, NULL)
  
})
