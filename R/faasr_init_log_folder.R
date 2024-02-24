#' @name faasr_init_log_folder
#' @title faasr_init_log_folder
#' @description 
#' This function creates an initial log folder in S3; it is called only at the entry point of a Workflow
#' The folder name is a concatenation of FaaSrLog and InvocationID from the Payload.
#' The folder name needs to be a unique ID. If the user doesn't provide InvocationID, generate a UUID
#' If the folder already exists, abort with an error
#' @param faasr list with parsed and validated Payload
#' @return faasr a list of json file, UUID updated.
#' @import uuid
#' @importFrom "paws.storage" "s3"
#' @keywords internal

faasr_init_log_folder <- function(faasr) {
  # if InvocationID doesn't have valid form, generate a UUID
  if (length(faasr$InvocationID) == 0) {
    faasr$InvocationID<-UUIDgenerate()
  } else if (UUIDvalidate(faasr$InvocationID) == FALSE) {
    faasr$InvocationID<-UUIDgenerate()
  }

  if (is.null(faasr$LoggingDataStore)){
    target_s3 <- faasr$DefaultDataStore
  } else {
    target_s3 <- faasr$LoggingDataStore
  }
      
  target_s3 <- faasr$DataStores[[target_s3]]
  s3<-paws.storage::s3(
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
  
  #If user didn't set FaaSrLog name, set it as a default, "FaaSrLog" 
  if (length(faasr$FaaSrLog)==0 || faasr$FaaSrLog==""){
    faasr$FaaSrLog <- "FaaSrLog"
  } 

  idfolder <- paste0(faasr$FaaSrLog,"/" ,faasr$InvocationID, "/")

  check_UUIDfolder<-s3$list_objects_v2(Prefix=idfolder, Bucket=target_s3$Bucket)
  if (length(check_UUIDfolder$Contents)!=0){
    err_msg <- paste0('{\"faasr_init_log_folder\":\"InvocationID already exists: ', faasr$InvocationID,'\"}', "\n")
    message(err_msg)
    stop()
  }else{
    s3$put_object(Key=idfolder, Bucket=target_s3$Bucket)
  }
  return(faasr)
}
