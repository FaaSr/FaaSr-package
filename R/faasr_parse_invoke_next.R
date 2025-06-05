#' @name faasr_parse_invoke_next_string
#' @title faasr_parse_invoke_next_string
#' @description 
#' Parse InvokeNext string to extract function name, boolean condition, and rank
#' Supports formats: "FuncName", "FuncName[TRUE]", "FuncName[FALSE]", "FuncName(rank)", "FuncName[TRUE](rank)"
#' @param invoke_string Single InvokeNext string entry
#' @return List with function, condition, and rank information
#' @keywords internal

faasr_parse_invoke_next_string <- function(invoke_string) {
  
  result <- list(func_name = NULL,
    condition = NULL,
    rank = 1)
  
  # First, extract condition from square brackets [TRUE] or [FALSE]
  condition_match <- regexpr("\\[(TRUE|FALSE)\\]", invoke_string, ignore.case = TRUE)
  if (condition_match[1] != -1) {
    # Extract condition value between square brackets
    condition_str <- regmatches(invoke_string, condition_match)
    condition_value <- gsub("\\[|\\]", "", condition_str)  # Remove brackets
    
    # RESTRICTION: Only allow TRUE or FALSE
    if (toupper(condition_value) == "TRUE") {
      result$condition <- TRUE
    } else if (toupper(condition_value) == "FALSE") {
      result$condition <- FALSE
    } else {
      # ERROR: Invalid condition - only TRUE/FALSE allowed
      err_msg <- paste0("Invalid condition: [", condition_value, "]. Only [TRUE] and [FALSE] are supported.")
      stop(err_msg)
    }
    
    # Remove the condition part from the string
    invoke_string <- gsub("\\[(TRUE|FALSE)\\]", "", invoke_string, ignore.case = TRUE)
  }
  
  # Then, extract rank from parentheses (rank) - existing logic
  rank_parts <- unlist(strsplit(invoke_string, "[()]"))
  result$func_name <- rank_parts[1]
  
  if (length(rank_parts) > 1) {
    rank_str <- trimws(rank_parts[2])
    if (grepl("^[0-9]+$", rank_str)) {
      result$rank <- as.numeric(rank_str)
    }
  }
  
  return(result)
}

#' @name faasr_evaluate_condition
#' @title faasr_evaluate_condition  
#' @description 
#' Evaluate if a boolean condition matches the function result
#' Only supports TRUE/FALSE conditions
#' @param condition Expected boolean condition (TRUE or FALSE)
#' @param result Actual function result
#' @return Boolean indicating if condition matches
#' @keywords internal

faasr_evaluate_condition <- function(condition, result) {
  if (is.null(condition)) {
    return(TRUE)  # No condition = always execute
  }
  
  # RESTRICTION: Only handle boolean conditions
  if (!is.logical(condition)) {
    stop("Condition must be TRUE or FALSE")
  }
  
  # Convert result to boolean if possible
  if (is.logical(result)) {
    return(condition == result)
  } else {
    # Try to interpret result as boolean
    if (is.character(result)) {
      if (toupper(result) == "TRUE") {
        return(condition == TRUE)
      } else if (toupper(result) == "FALSE") {
        return(condition == FALSE)
      }
    } else if (is.numeric(result)) {
      # Treat 0 as FALSE, everything else as TRUE
      return(condition == (result != 0))
    }
    
    # If we can't interpret as boolean, log warning and don't match
    warn_msg <- paste0("Function result '", result, "' cannot be interpreted as TRUE/FALSE")
    message(warn_msg)
    return(FALSE)
  }
}