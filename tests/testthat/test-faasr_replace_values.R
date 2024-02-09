test_that("replace_values", {
  ## This test is for find that replace_values function property replace real credential values
  ## to the fake credential values
  ## Inputs are "faasr" and "cred" lists
  faasr <- list()
  cred <- list()
  
  ## faasr needs either or both "ComputeServers" and "DataStores".
  ## In "ComputeServers", "FaaSType" and each key type should be defined.
  ## Note that key value should be combination of "server name" and "key type in capital" 
  faasr$ComputeServers$IBM_Account$FaaSType <- "OpenWhisk"
  faasr$ComputeServers$IBM_Account$API.key <- "IBM_Account_API_KEY"
  
  ## Data store servers are same. But it only needs the key type.
  faasr$DataStores$S3_Account$AccessKey <- "S3_Account_ACCESS_KEY"
  faasr$DataStores$S3_Account$SecretKey <- "S3_Account_SECRET_KEY"
  
  ## cred needs real keys
  cred$IBM_Account_API_KEY <- "real_api_key"
  cred$S3_Account_ACCESS_KEY <- "real_s3_access_key"
  cred$S3_Account_SECRET_KEY <- "real_s3_secret_key"
  
  ## execute the function and stors the result
  result <- faasr_replace_values(faasr, cred)
  
  ## Check the result
  expect_equal(result$ComputeServers$IBM_Account$API.key, "real_api_key")
  expect_equal(result$DataStores$S3_Account$AccessKey, "real_s3_access_key")
  expect_equal(result$DataStores$S3_Account$SecretKey, "real_s3_secret_key")
})
