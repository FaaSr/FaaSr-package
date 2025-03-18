#' @name faasr_arrow_s3_bucket
#' @title faasr_arrow_s3_bucket
#' @description 
#' `test` Uses "arrow" library to set up the configurations with given json file and 
#' provide the object to the users
#' @param server_name for string, default value is faasr$DefaultDataStore
#' @param faasr_config optional configuration to use instead of global .faasr
#' @return s3 representing object for "arrow"
#' @export
#' @examples
#' # this function can be run only inside the container
#' if (interactive()){
#' arrow_s3 <- faasr_arrow_s3_bucket()
#' arrow_s3$ls
#' }

globalVariables(".faasr")

faasr_arrow_s3_bucket <- function(server_name=.faasr$DefaultDataStore, faasr_prefix="",faasr_config=NULL) {
  
  if (!is.null(faasr_config)) {
    # Use the provided config
    config <- faasr_config
    server_name <- config$DefaultDataStore
    
  } else {
    # Use the original behavior with global .faasr
    config <- .faasr
    
  }
  
  # Check that an S3 server_name has been defined
  # If not, log an error and abort

  if (server_name %in% names(config$DataStores)) {
    NULL
  } else {
    err_msg <- paste0('{\"faasr_get_arrow\":\"Invalid data server name: ',server_name,'\"}', "\n")
    message(err_msg)
    stop()
  }

  target_s3 <- config$DataStores[[server_name]]

  if (faasr_prefix != ""){
    bucket <- paste0(target_s3$Bucket, "/", faasr_prefix)
  } else {
    bucket <- target_s3$Bucket
  }

  if (is.null(target_s3$Anonymous)){
    faasr_anonymous <- FALSE
  } else {
    faasr_anonymous <- as.logical(target_s3$Anonymous) 
  }  

  if (faasr_anonymous){
    s3 <- arrow::s3_bucket(
      bucket = bucket,
      endpoint_override = target_s3$Endpoint,
      region = target_s3$Region,
      anonymous = TRUE
    )
  } else {
    s3 <- arrow::s3_bucket(
      bucket = bucket,
      access_key = target_s3$AccessKey,
      secret_key = target_s3$SecretKey,
      endpoint_override = target_s3$Endpoint,
      region = target_s3$Region
    )
  }

  return(s3)
}
