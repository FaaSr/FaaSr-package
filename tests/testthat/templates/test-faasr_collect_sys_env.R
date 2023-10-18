test_that("collect_system_environment", {
  ## this function is to get the system envrionment, which is the credentials.
  ## Inputs are "faasr" and "cred" lists
  ## Sys.setenv() should be done before testing.
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
  
  ## Set environment for the function
  Sys.setenv(IBM_Account_API_KEY="real_ibmcloud_api_key")
  Sys.setenv(S3_Account_ACCESS_KEY="real_s3_access_key")
  Sys.setenv(S3_Account_SECRET_KEY="real_s3_secret_key")
  
  ## execute the function and store the result
  result <- faasr_collect_sys_env(faasr,cred)
  
  ## check the result - result[key] is same as cred[key]
  expect_equal(result$IBM_Account_API_KEY, "real_ibmcloud_api_key")
  expect_equal(result$S3_Account_ACCESS_KEY, "real_s3_access_key")
  expect_equal(result$S3_Account_SECRET_KEY, "real_s3_secret_key")
  
  
  ###########################################################################
  ###########################################################################
  ## Now we can consider the case where some of credentials are already provided.
  ## Environment should not overwrite it.
  cred$IBM_Account_API_KEY <- "123"
  
  ## Re-set environment for the function
  Sys.setenv(IBM_Account_API_KEY="real_ibmcloud_api_key")
  Sys.setenv(S3_Account_ACCESS_KEY="real_s3_access_key")
  Sys.setenv(S3_Account_SECRET_KEY="real_s3_secret_key")
  
  ## execute the function and store the result
  result <- faasr_collect_sys_env(faasr,cred)
  
  ## check the result - result[key] is same as cred[key]
  ## we can expect that, at this time, "IBM_Account_API_KEY" in the result should be "123"
  expect_equal(result$IBM_Account_API_KEY, "123")
  expect_equal(result$S3_Account_ACCESS_KEY, "real_s3_access_key")
  expect_equal(result$S3_Account_SECRET_KEY, "real_s3_secret_key")
})
