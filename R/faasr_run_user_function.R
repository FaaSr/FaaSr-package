#' @name faasr_run_user_function
#' @title faasr_run_user_function
#' @description 
#' Run user functions and leave the state information
#' @param .faasr list with parsed and validated Payload
#' @export

faasr_run_user_function <- function(.faasr){ 
  
  # If the Action reaches this point without aborting, it is ready to invoke the User Function
  # Extract the name of the User Function from the Payload, and invoke it, passing the parsed Payload as arg
  # try get(faasr$FunctionInvoke) and if there's an error, return error message and stop the function
  func_name <- .faasr$FunctionList[[.faasr$FunctionInvoke]]$FunctionName
  user_function = tryCatch(expr=get(func_name), error=function(e){
    err_msg <- paste0('{\"faasr_user_function\":\"Cannot find Function ',func_name,', check the name and sources\"}', "\n")
    cat(err_msg)
    result <- faasr_log(err_msg)
    stop()
    }
  )
  
  # Get a list of the current function's arguments.
  user_args = faasr_get_user_function_args(.faasr)
  
  # Use do.call to use user_function with arguments
  # try do.call and if there's an error, return error message and stop the function
  faasr_result <- tryCatch(expr=do.call(user_function, user_args), error=function(e){
    nat_err_msg <- paste0('\"faasr_user_function\":Errors in the user function ', as.character(e))
    err_msg <- paste0('{\"faasr_user_function\":\"Errors in the user function: ',.faasr$FunctionInvoke,', check the log for the detail \"}', "\n")
    result_2 <- faasr_log(nat_err_msg)
    cat(err_msg)
    stop()
    }
  )

  # At this point, the Action has finished the invocation of the User Function
  # We flag this by uploading a file with name FunctionInvoke.done with contents TRUE to the S3 logs folder
  # Check if directory already exists. If not, create one
  log_folder <- paste0(.faasr$FaaSrLog,"/",.faasr$InvocationID)
  if (!dir.exists(log_folder)) {
    dir.create(log_folder, recursive=TRUE)
  }
  file_name <- paste0(.faasr$FunctionInvoke, ".done")
  write.table("TRUE", file=paste0(log_folder, "/", file_name), row.names=F, col.names=F)
  faasr_put_file(local_folder=log_folder, local_file=file_name, remote_folder=log_folder, remote_file=file_name)
}
