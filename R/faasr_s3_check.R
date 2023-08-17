faasr_s3_check <- function(faasr){

  for(server in names(faasr$DataStores)){
    endpoint_check <- faasr$DataStores[[server]]$Endpoint
    region_check <- faasr$DataStores[[server]]$Region
    if (length(endpoint_check)==0 || endpoint_check=="") {
      faasr$DataStores[[server]]$Endpoint <- ""
    }else{
      if (!(startsWith(endpoint_check, "http"))){
        msg <- paste0('{\"faasr_s3_check\":\"Invalid Logging server endpoint ',server,'\"}', "\n")
        cat(msg)
        stop()
      }
    }
    s3<-paws::s3(
      config=list(
	      credentials=list(
	        creds=list(
		        access_key_id=faasr$DataStores[[server]]$AccessKey,
		        secret_access_key=faasr$DataStores[[server]]$SecretKey
		      )
	      ),
	    endpoint=faasr$DataStores[[server]]$Endpoint,
	    region=faasr$DataStores[[server]]$Region
	  )
  )
    check <- try(s3$list_buckets(), silent=TRUE)
    if(class(check)==list){
      bucket_names <- lapply(check$Buckets, function(bucket) bucket$Name)
      if(!(faasr$DataStores[[server]]$Bucket %in% bucket_names)){
        msg <- paste0('{\"faasr_s3_check\":\"S3 server ',server,' failed with message: No such bucket\"}', "\n")
        cat(msg)
        stop()
      }
    }else{
      msg <- paste0('{\"faasr_s3_check\":\"S3 server ',server,' failed with message: Data store server unreachable\"}', "\n")
      cat(msg)
      stop()
    }
  }
  return(faasr)
}
