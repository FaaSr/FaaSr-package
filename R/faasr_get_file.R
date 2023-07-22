#' @title Gets a file from an S3 bucket
#' @description Helper function to download a file from an S3 bucket to local Action folder
#' @param faasr list with parsed and validated Payload
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param remote_folder string with the name of the remote folder where the file is to be downloaded from
#' @param remote_file string with the name for the file to be downloaded from the S3 bucket
#' @param local_folder string with the name of the local folder where the file to be downloaded is stored
#' @param local_file string with the name of the local file once downloaded

library("paws")

faasr_get_file <- function(faasr, server_name, remote_folder, remote_file, local_folder, local_file) {
  # Check that an S3 server_name has been defined
  # If not, log an error and abort
  if (server_name %in% names(faasr$DataStores)) {
    NULL
   } else {
     err_msg <- paste0('{\"faasr_get_file\":\"Invalid data server name: ',server_name,'\"}', "\n")
     cat(err_msg)
     stop()	
   }

  target_s3 <- faasr$DataStores[[server_name]]
  get_file <- paste0(local_folder,"/",local_file)
  get_file_s3 <- paste0(remote_folder, "/", remote_file)
  if (!dir.exists(local_folder)) {
    dir.create(local_folder, recursive=TRUE)
  }

  s3 <- paws::s3(
    config=list(
	  credentials=list(
	    creds=list(
		  access_key_id=target_s3$AccessKey,
		  secret_access_key=target_s3$SecretKey
		)
	  ),
	  region=target_s3$Region
	)
  )

  if (file.exists(get_file)) {
    file.remove(get_file)
  }

  result <- s3$download_file(Key=get_file_s3, Filename=get_file, Bucket=target_s3$Bucket)
}
