# TBD

library("uuid")
library("paws")

faasr_init_log_folder <- function(faasr) {
		if (length(faasr$InvocationID) == 0) {
		  faasr$InvocationID<-UUIDgenerate()
  		# if InvocationID doesn't have valid form, generate a UUID
  		} else if (UUIDvalidate(faasr$InvocationID) == FALSE) {
  		  faasr$InvocationID<-UUIDgenerate()
  		}

		target_s3 <- faasr$LoggingServer
		target_s3 <- faasr$DataStores[[target_s3]]
		Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region, "AWS_SESSION_TOKEN" = "")
		s3<-paws::s3(
	  		config=list(
		  		credentials=list(
			  		creds=list(
				  		access_key_id=target_s3$AccessKey,
				  		secret_access_key=target_s3$SecretKey)),
		  		region=target_s3$Region)
	  	)

		idfolder <- paste0(faasr$FaaSrLog,"/" ,faasr$InvocationID, "/")

		check_UUIDfolder<-s3$list_objects_v2(Prefix=idfolder, Bucket=target_s3$Bucket)
		#if (object_exists(idfolder, target_s3$Bucket)){
		#	cat('{\"msg\":\"InvocationID already exists\"}', "\n")
		#	stop()
		#} else { put_folder(faasr$InvocationID, bucket=target_s3$Bucket) }
		if (length(check_UUIDfolder$Contents)!=0){
			cat('{\"msg\":\"faasr_init_log_folder: InvocationID already exists\"}', "\n")
			stop()
		} else {
		  s3$put_object(Key=idfolder, Bucket=target_s3$Bucket)
		}
