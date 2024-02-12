#' @name faasr_delete_file
#' @title faasr_delete_file
#' @description Helper function to delete a file in an S3 bucket
#' @param faasr list with parsed and validated Payload
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param remote_folder string with the name of the remote folder where the file is to be deleted from
#' @param remote_file string with the name for the file to be deleted 
#' @import paws
#' @export

globalVariables(".faasr")

# Default server_name is DefaultDataStore, default remote folder name is empty ("") and 
# local folder name is current directory(".")
faasr_delete_file <- function(server_name=.faasr$DefaultDataStore, remote_folder="", remote_file) { 
  # Check that an S3 server_name has been defined
  # If not, log an error and abort
  
  if (server_name %in% names(.faasr$DataStores)) {
    NULL
   } else {
     err_msg <- paste0('{\"faasr_delete_file\":\"Invalid data server name: ',server_name,'\"}', "\n")
     cat(err_msg)
     stop()	
   }

  target_s3 <- .faasr$DataStores[[server_name]]

  # Remove "/" in the folder & file name to avoid situations:
  # 1: duplicated "/" ("/remote/folder/", "/file_name") 
  # 2: multiple "/" by user mistakes ("//remote/folder//", "file_name")
  # 3: file_name ended with "/" ("/remote/folder", "file_name/")
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  delete_file_s3 <- paste0(remote_folder, "/", remote_file)

  s3 <- paws::s3(
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

  result <- s3$delete_object(Key=delete_file_s3, Bucket=target_s3$Bucket)
}
