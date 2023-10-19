test_json <- readLines("test.json")
  

test_that("parse", {
  ## "parse" validates the given JSON payload and returns the list of JSON.
  ## input should be JSON format (not a list at this time)
  
  ## 1. it validates JSON itself. 
  ## Write a simple json which is incorrect. In this example, there's an extra "
  faasr <- '{\"test\":\"abc\"\"}'
  # We can expect error from here
  expect_error(faasr_parse(faasr))
    
  ## 2. it valiates JSON with proper schema.
  ## schema can be found in here: https://github.com/FaaSr/FaaSr-package/blob/main/schema/FaaSr.schema.json
  ## you can find the json format here: https://github.com/spark0510/FaaSr-package/blob/branch21-unreachable/docs/setting.md#json-format
  
  ## use test.json in this example. This has no error.
  expect_no_error(faasr_parse(test_json))
  
  #################################################################################
  #################################################################################
  ## Now we should modify test_json for error detecting.
  ## You can make it as a list and edit it, as follows:
  json_list <- jsonlite::fromJSON("test.json")
  ## intentionally make an error: FaaSType should be Github but set it up as Lambda
  json_list$ComputeServers$My_Github_Account$FaaSType <- "Lambda"
  ## Turn the list back to the JSON forma
  test_json <- jsonlite::toJSON(json_list, auto_unbox=TRUE)
  
  ## We can expect errors at this time
  expect_error(faasr_parse(test_json))
  
  #################################################################################
  ## You should modify test_json as many times as you can to find errors.
  
})
