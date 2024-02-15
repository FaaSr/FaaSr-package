#' @name faasr_get_file
#' @title faasr_get_file
#' @description 
#' Helper function to download a file from an S3 bucket to local Action folder
#' @param faasr list with parsed and validated Payload
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param remote_folder string with the name of the remote folder where the file is to be downloaded from
#' @param remote_file string with the name for the file to be downloaded from the S3 bucket
#' @param local_folder string with the name of the local folder where the file to be downloaded is stored
#' @param local_file string with the name of the local file once downloaded
#' @return return nothing / delete the file in the bucket
#' @importFrom "paws.storage" "s3"
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' faasr_get_file(remote_folder="test", remote_file="test.txt", local_folder="test", local_file="test.txt")
#' }

globalVariables(".faasr")

# Default server_name is DefaultDataStore, default remote folder name is empty ("") and 
# local folder name is current directory(".")
faasr_get_file <- function(server_name=.faasr$DefaultDataStore, remote_folder="", remote_file, local_folder=".", local_file) { 
  # Check that an S3 server_name has been defined
  # If not, log an error and abort
  
  if (server_name %in% names(.faasr$DataStores)) {
    NULL
   } else {
     err_msg <- paste0('{\"faasr_get_file\":\"Invalid data server name: ',server_name,'\"}', "\n")
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
  get_file_s3 <- paste0(remote_folder, "/", remote_file)

  # takes same way with remote folder & file
  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  get_file <- paste0(local_folder,"/",local_file)
   
  if (!dir.exists(local_folder)) {
    dir.create(local_folder, recursive=TRUE)
  }

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

  if (file.exists(get_file)) {
    file.remove(get_file)
  }

  result <- s3$download_file(Key=get_file_s3, Filename=get_file, Bucket=target_s3$Bucket)
}
