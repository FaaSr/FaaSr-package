library("arrow")


faasr_arrow_s3_bucket <- function(server_name=.faasr$DefaultDataStore) {
  # Check that an S3 server_name has been defined
  # If not, log an error and abort

  if (server_name %in% names(.faasr$DataStores)) {
    NULL
  } else {
    err_msg <- paste0('{\"faasr_get_arrow\":\"Invalid data server name: ',server_name,'\"}', "\n")
    cat(err_msg)
    stop()
  }

  target_s3 <- .faasr$DataStores[[server_name]]


  s3 <- arrow::s3_bucket(
    bucket = server_name,
    access_key = target_s3$AccessKey,
    secret_key = target_s3$SecretKey,
    endpoint_override = target_s3$Endpoint
  )

  return(s3)
}
