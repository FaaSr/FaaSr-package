#' @title Extracts the arguments for the User Function
#' @description Helper function to extract arguments of a User Function from the faasr parsed Payload list
#' @param faasr list with parsed and validated Payload
#' @return args list of User Function arguments

faasr_get_user_function_args <- function(faasr) {
  # First extract the name of the User Function to invoke
  user_action = faasr$FunctionInvoke

  # Now extract and return the arguments specific to this User Function
  args = faasr$FunctionList[[user_action]]$Arguments
  if (is.null(args)){
    return(list())
  }
  return(args)
}
