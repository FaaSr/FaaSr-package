#' @title Puts a file into S3 bucket
#' @description Helper function to upload a file from a local Action folder to an S3 bucket
#' @param faasr list with parsed and validated Payload
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param local_folder string with the name of the local folder where the file to be uploaded resides
#' @param local_file string with the name of the local file to be uploaded
#' @param remote_folder string with the name of the remote folder where the file is to be uploaded to
#' @param remote_file string with the name for the file once uploaded to the S3 bucket

library("paws")

faasr_put_file <- function(faasr, server_name, local_folder, local_file, remote_folder, remote_file) {

  # Check that an S3 server_name has been defined
  # If not, log an error and abort
  if (server_name %in% names(faasr$DataStores)) {
    NULL
  } else {
    cat('{\"msg\":\"faasr_put_file: Invalid data server name\"}', "\n")
    stop()
  }

  target_s3 <- faasr$DataStores[[server_name]]
  put_file <- paste0(local_folder,"/",local_file)
  put_file_s3 <- paste0(remote_folder, "/", remote_file)

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
  result <- s3$put_object(Body=put_file, Key=put_file_s3, Bucket=target_s3$Bucket)
}

