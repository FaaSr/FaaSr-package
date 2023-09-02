#' @title Gets a file from an S3 bucket
#' @description Helper function to download a file from an S3 bucket to local Action folder
#' @param faasr list with parsed and validated Payload
#' @param server_name string with name of the S3 bucket to use; must match a name declared in the faasr list
#' @param remote_folder string with the name of the remote folder where the file is to be downloaded from
#' @param remote_file string with the name for the file to be downloaded from the S3 bucket
#' @param local_folder string with the name of the local folder where the file to be downloaded is stored
#' @param local_file string with the name of the local file once downloaded

library("paws")

# Default server_name is Logging Server, default remote folder name is empty ("") and 
# local folder name is current directory(".")
faasr_get_file <- function(server_name=.faasr$LoggingServer, remote_folder="", remote_file, local_folder=".", local_file) {
  # Define "faasr" by using global variable ".faasr" 
  faasr <- .faasr
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

  # Remove "/" in the folder & file name to avoid situations:
  # 1: duplicated "/" ("/remote/folder/", "/file_name") 
  # 2: multiple "/" by user mistakes ("//remote/folder//", "file_name")
  # 3: file_name ended with "/" ("/remote/folder", "file_name/")
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  get_file_s3 <- paste0(remote_folder, "/", remote_file)

  # Check the situation that local_path is not defined and local_folder is defined as an absoulte path.
  if (local_folder="." && local_file == normalizePath(local_file)){
    local_folder <- dirname(local_file)
    get_file <- local_file
  # If not, takes same way with remote folder & file
  } else{
    local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
    local_file <- sub("^/+", "", sub("/+$", "", local_file))
    get_file <- paste0(local_folder,"/",local_file)
  }  
  
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
	  endpoint=target_s3$Endpoint,
	  region=target_s3$Region
	)
  )

  if (file.exists(get_file)) {
    file.remove(get_file)
  }

  result <- s3$download_file(Key=get_file_s3, Filename=get_file, Bucket=target_s3$Bucket)
}
