test_that("faasr_function", {
  ## this function is to set a configuration
  ## input is JSON file path
  ## outputs are various: svc configurations, svc$json, svc$cred, svc$json_path, directory("/.faasr_json/random number file)
  faasr_std <- jsonlite::fromJSON("test.json")
  
  ## We're using our "test.json" file
  faasr_test <- faasr(json_path="test.json")
  
  ## 1. Check credentials are set as "server name" + "key type in capital letter"
  expect_equal(faasr_test$json$ComputeServers$My_Github_Account$Token, "My_Github_Account_TOKEN")
  expect_equal(faasr_test$json$ComputeServers$My_AWS_Account$AccessKey, "My_AWS_Account_ACCESS_KEY")
  expect_equal(faasr_test$json$ComputeServers$My_IBM_Account$API.key, "My_IBM_Account_API_KEY")
  
  ## 2. Check and compare json content
  ## Note that comparing ComputeServers/DataStores would be failed because "faasr" function replaces
  ## all the credentials with "server name" + "key type in capital letter"
  expect_equal(faasr_test$json$FunctionInvoke, faasr_std$FunctionInvoke)
  expect_equal(faasr_test$json$ActionContainers, faasr_std$ActionContainers)
  
  
  ## 3. Check json_path is not NULL
  ## json_path isn't NULL, so first wrap it up with "expect_null" function
  ## Then, use "expect_failure" to say that "expect_null" function will be failed.
  expect_failure(expect_null(faasr_test$json_path))
  
  ## 4. Check json_file and directory is created.
  ## file would have been created on the path: faasr_test$json_path
  expect_true(file.exists(faasr_test$json_path))
  
  ## Change the json files and test it.

  ## After all the test, clean up the directory
  unlink(".faasr_json", recursive=TRUE, force=TRUE)
})
