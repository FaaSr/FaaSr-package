#' @title Appends to the user's log file in an S3 bucket
#' @description Helper function to append to the log file residing in an S3 bucket
#' the name of the S3 server is implicit from the validated JSON payload, key LoggingServer
#' the log file is a concatenation of folder "logs" and file name "faasr_log_" + InvocationID + ".txt"
#' @param faasr list with parsed and validated Payload
#' @param log_message string message to be appended to the log

library("paws")

faasr_log <- function(log_message) {
  # set file name to be "faasr_log_" + faasr$InvocationID + faasr$FunctionInvoke + "_local.txt"
  log_folder <- paste0(.faasr$FaaSrLog, "/", .faasr$InvocationID)
  log_file_local <- paste0(log_folder, "/", .faasr$FunctionInvoke,"_local.txt")
  if (!dir.exists(log_folder)){dir.create(log_folder, recursive=TRUE)}

  # only write the log for the local file.
  logs <- log_message
  write.table(logs, log_file_local, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)
}

faasr_log_flush <- function(){
  if (is.null(.faasr$LoggingDataStore)){
    log_server_name = .faasr$DefaultDataStore
  } else {
    log_server_name = .faasr$LoggingDataStore
  }
  # Validate that server_name exists, otherwise abort
  if (log_server_name %in% names(.faasr$DataStores)) {
    NULL
  } else {
    err_msg <- paste0('{\"faasr_log\":\"Invalid logging server name: ',log_server_name,'\"}', "\n")
    cat(err_msg)
    stop()
  }
  
  log_server <- .faasr$DataStores[[log_server_name]]
  
  s3<-paws::s3(
    config=list(
      credentials=list(
        creds=list(
          access_key_id=log_server$AccessKey,
          secret_access_key=log_server$SecretKey
        )
      ),
      endpoint=log_server$Endpoint,
      region=log_server$Region
    )
  )
  
  # set file name to be "faasr_log_" + faasr$InvocationID + faasr$FunctionInvoke + ".txt"
  log_folder <- paste0(.faasr$FaaSrLog, "/", .faasr$InvocationID)
  log_file <- paste0(log_folder, "/", .faasr$FunctionInvoke,".txt")
  log_file_local <- paste0(log_folder, "/", .faasr$FunctionInvoke,"_local.txt")
  if (file.exists(log_file_local)){
    logs_local <- read.table(log_file_local)
    
    if (!dir.exists(log_folder)){dir.create(log_folder, recursive=TRUE)}
    
    check_log_file <- s3$list_objects_v2(Bucket=log_server$Bucket, Prefix=log_file)
    if (length(check_log_file$Contents) != 0) {
      if (file.exists(log_file)) {
        result <- file.remove(log_file)
      }
      result <- s3$download_file(Bucket=log_server$Bucket, Key=log_file, Filename=log_file)
    }
     # append message to the local file then upload
    result <- write.table(logs_local, log_file, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)
    result <- s3$put_object(Body=log_file, Key=log_file, Bucket=log_server$Bucket)
    result <- file.remove(log_file_local)
  }
  
}

