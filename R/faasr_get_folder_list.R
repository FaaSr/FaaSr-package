#' @name faasr_get_folder_list
#' @title faasr_get_folder_list
#' @description Helper function to get a list of objects in an S3 bucket
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param faasr_prefix string with prefix of objects in the S3 bucket
#' @param faasr_config optional configuration to use instead of global .faasr
#' @return object_list a list of objects in the bucket
#' @importFrom "paws.storage" "s3"
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' faasr_get_folder_list(server_name="My_Minio_Bucket")
#' }

globalVariables(".faasr")

# Default server_name is DefaultDataStore, default prefix is ""
faasr_get_folder_list <- function(server_name=.faasr$DefaultDataStore, faasr_prefix="",faasr_config=NULL) { 

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
     err_msg <- paste0('{\"faasr_get_folder_list\":\"Invalid data server name: ',server_name,'\"}', "\n")
     message(err_msg)
     stop()	
   }

  target_s3 <- config$DataStores[[server_name]]

  s3 <- paws.storage::s3(
    config=list(
	  credentials=list(
	    creds=list(
		  access_key_id=target_s3$AccessKey,
		  secret_access_key=target_s3$SecretKey
		)
	  ),
	  endpoint=target_s3$Endpoint,
	  region=target_s3$Region
	)
  )

  # return the result of a list containing only file-style objects in the bucket.
  result <- s3$list_objects_v2(Bucket=target_s3$Bucket, Prefix=faasr_prefix)
  result <- lapply(result$Contents, function(x) x$Key)
  result <- result[!grepl("/$", result)]
  return(result)
}
