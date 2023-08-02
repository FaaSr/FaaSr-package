#' @title Parses and validates JSON Payload for a FaaSr invocation
#' @description
#' This function uses JSON parsing and validation to ensure the Payload is compliant
#' Two checks are made here: 1) is it a valid JSON format? and 2) does it conform to the FaaSr JSON schema?
#' If both checks pass, return a list with all the parsed key/value pairs
#' Otherwise, abort
#' @param faasr_payload JSON Payload provided upon Action invocation by the FaaS platform
#' @return faasr list with parsed and validated Payload

library("jsonlite")
library("jsonvalidate")

faasr_parse <- function(faasr_payload) {
  faasr_schema <- readLines("FaaSr.schema.json")

  # Use json_validator: make faasr_schema_valid as a validator. ajv should be used as an engine.
  faasr_schema_valid <- json_validator(faasr_schema, engine="ajv")

  # If Payload is valid JSON, do nothing; otherwise, log error to standard output and abort
  if (validate(faasr_payload)) {
    NULL
  } else {
	log <- attr(validate(faasr_payload),"err")
	err_msg <- paste0('{\"faasr_parse\":\"Invalid JSON Payload: ',log,'\"}', "\n")
	cat(err_msg)
	stop()
  }

  faasr <- fromJSON(faasr_payload)
  # Schema check - if it returns TRUE, return faasr
  if (faasr_schema_valid(faasr_payload)) {
    return(faasr)
  } else {
	#err_msg <- paste0('{\"faasr_parse\":\"JSON Payload not compliant with FaaSr schema\"}', "\n")
	#cat(err_msg)
	  
	message_schema <- attr(faasr_schema_valid(faasr_payload, verbose=TRUE, greedy=TRUE),"errors")
        tag <- c("schemaPath", "message")
        log <- message_schema[,tag]
        log_s <- paste(log$schemaPath, log$message, "\n", sep = " ")
        cat(log_s)
	err_msg <- paste0('{\"faasr_parse\":\"JSON Payload error - please check the logs for your FaaS provider for more information\"}', "\n")
	cat(err_msg)
        stop()

  }
}

