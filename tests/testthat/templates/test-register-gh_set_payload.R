test_that("github_set_payload", {
  ## this function makes a file named "payload.json" with given faasr
  ## let's use test.json
  faasr <- jsonlite::fromJSON("test.json")
  
  ## execute the function
  faasr_register_workflow_github_set_payload(faasr)
  
  ## check whether it made a file named "payload.json"
  expect_true(file.exists("payload.json"))
  
  #####################################################################
  #####################################################################
  ## Now let's modify the "faasr"
  faasr$FunctionInvoke <- "some_function"
  
  ## execute the function
  faasr_register_workflow_github_set_payload(faasr)
  
  ## read the "payload.json" file
  faasr_new <- jsonlite::fromJSON("payload.json")
  
  ## Check it is modified.
  expect_equal(faasr_new$FunctionInvoke, faasr$FunctionInvoke)
  
  
  ## After all the tests, delete the "payload.json"
  file.remove("payload.json")
})
