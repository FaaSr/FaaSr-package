test_that("register_lambda_function_images", {
  ## this functions creates a list of [actionname:action container image] pairs
  ## input is a list "faasr".
  ## "faasr" requires "FunctionList", "ComputeServers", and "ActionContainers"
  ## In "FunctionList", "FaaSServer" and "Actionname" should be defined.
  faasr <- list()
  faasr$FunctionList$F1$FaaSServer <- "Lambda_Account"
  faasr$FunctionList$F2$FaaSServer <- "Lambda_Account"
  faasr$FunctionList$F3$FaaSServer <- "Lambda_Account"
  
  faasr$FunctionList$F1$Actionname <- "F1_action"
  faasr$FunctionList$F2$Actionname <- "F2_action"
  faasr$FunctionList$F3$Actionname <- "F3_action"
  
  ## In "ComputeServers", "FaaSType" should be defined with the name matched.
  faasr$ComputeServers$Lambda_Account$FaaSType <- "Lambda"
  
  ## In "ActionContainers", key:value [Actionname:Container image] pairs should be defined.
  ## This is optional because the function will automatically set the default image if the pair is "NA".
  ## In this example, I'll only write function "F1"'s actioncontainer image.
  faasr$ActionContainers$F1_action <- "My_Container_Image"
  
  ## execute the function with arguments and store the result in the variable
  result <- faasr_register_workflow_lambda_function_image(faasr)
  
  ## Check the result by picking up some values
  expect_equal(result$F1_action, "My_Container_Image")
  expect_equal(result$F2_action, "faasr/aws-lambda-tidyverse")
  
  
  ## You can also consider other FaaS types.
  ## For example, I'll set "F3"'s FaaSServer as "GitHubActions_Account" which is "GitHubActions"
  faasr$FunctionList$F3$FaaSServer <- "GitHubActions_Account"
  faasr$ComputeServers$GitHubActions_Account$FaaSType <- "GitHubActions"
  ## And set the action container as well
  faasr$ActionContainers$F3_action <- "My_Container_Image_F3"
  
  ## execute the function with arguments and store the result in the variable
  result <- faasr_register_workflow_lambda_function_image(faasr)
  
  ## Check the result
  ## We can expect that the list "result" has nothing related to "F3" as it is for "GitHubActions"
  ## So, result will be NULL
  expect_equal(result$F3_action, NULL)
})
