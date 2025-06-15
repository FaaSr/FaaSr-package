#' @name faasr_parse
#' @title faasr_parse
#' @description
#' This function uses JSON parsing and validation to ensure the Payload is compliant
#' Two checks are made here: 1) is it a valid JSON format? and 2) does it conform to the FaaSr JSON schema?
#' If both checks pass, return a list with all the parsed key/value pairs
#' Otherwise, abort
#' @param faasr_payload JSON Payload provided upon Action invocation by the FaaS platform
#' @return faasr list with parsed and validated Payload
#' @import jsonlite
#' @import jsonvalidate
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' faasr <- faasr_parse(faasr_payload)
#' }


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
	message(err_msg)
	stop()
  }

  faasr <- fromJSON(faasr_payload)
  # Schema check - if it returns TRUE, return faasr
  if (faasr_schema_valid(faasr_payload)) {
    faasr <- faasr_validate_conditional_triggers(faasr)
    return(faasr)
  } else {
	#err_msg <- paste0('{\"faasr_parse\":\"JSON Payload not compliant with FaaSr schema\"}', "\n")
	#message(err_msg)
	  
	message_schema <- attr(faasr_schema_valid(faasr_payload, verbose=TRUE, greedy=TRUE),"errors")
        tag <- c("schemaPath", "message")
        log <- message_schema[,tag]
        log_s <- paste(log$schemaPath, log$message, "\n", sep = " ")
        message(log_s)
	err_msg <- paste0('{\"faasr_parse\":\"JSON Payload error - please check the logs for your FaaS provider for more information\"}', "\n")
	message(err_msg)
        stop()
  }
}

faasr_validate_conditional_triggers <- function(faasr) {
  for (func_name in names(faasr$FunctionList)) {
    invoke_next <- faasr$FunctionList[[func_name]]$InvokeNext
    
    if (!is.null(invoke_next) && length(invoke_next) > 1) {
      # Check if any actions have predicates
      has_predicates <- any(grepl("\\[(TRUE|FALSE)\\]", invoke_next))
      has_non_predicates <- any(!grepl("\\[(TRUE|FALSE)\\]", invoke_next))
      
      # If we have both predicates and non-predicates, that's an error
      if (has_predicates && has_non_predicates) {
        err_msg <- paste0('{\"faasr_parse\":\"Function ', func_name, 
                          ' has mixed conditional and unconditional triggers. ',
                          'All InvokeNext actions must either have predicates [TRUE/FALSE] or none at all\"}', "\n")
        message(err_msg)
        stop()
      }
      
      # If using predicates, ensure all possible outcomes are covered
      if (has_predicates) {
        predicates <- stringr::str_extract_all(invoke_next, "\\[(TRUE|FALSE)\\]")
        predicates <- unlist(predicates)
        predicates <- gsub("[\\[\\]]", "", predicates)
        
        # Check if both TRUE and FALSE are covered
        if (!("TRUE" %in% predicates && "FALSE" %in% predicates)) {
          warning_msg <- paste0('{\"faasr_parse\":\"Function ', func_name, 
                                ' uses conditional triggers but does not cover both TRUE and FALSE outcomes\"}', "\n")
          message(warning_msg)
        }
      }
    }
  }
  
  return(faasr)
}

