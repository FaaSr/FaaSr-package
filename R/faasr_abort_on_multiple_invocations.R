# TBD

library("uuid")
library("paws")

faasr_abort_on_multiple_invocations <- function(faasr, pre) {

		# Set env for checking
		log_server_name = faasr$LoggingServer
		log_server <- faasr$DataStores[[log_server_name]]
		#Sys.setenv("AWS_ACCESS_KEY_ID"=log_server$AccessKey, "AWS_SECRET_ACCESS_KEY"=log_server$SecretKey, "AWS_DEFAULT_REGION"=log_server$Region, "AWS_SESSION_TOKEN" = "")
		s3<-paws::s3(
	  		config=list(
		  		credentials=list(
			  		creds=list(
				  		access_key_id=log_server$AccessKey,
				  		secret_access_key=log_server$SecretKey)),
		  		region=log_server$Region)
	  	)

		idfolder <- paste0(faasr$FaaSrLog,"/",faasr$InvocationID, "/")

		#check all "predecessorname.done" exists. If TRUE, it passes, elif FALSE, it stops
		for (func in pre) {

			# check filename is "functionname.done"
			file_names <- paste0(idfolder,"/",func,".done")
			check_fn_done<-s3$list_objects_v2(Bucket=log_server$Bucket, Prefix=file_names)
			# if object exists, do nothing.
			#if (object_exists(file_names, log_server$Bucket)){
			#	NULL
			#} else{
			#	faasr_log(faasr, "error:function should wait")
			#	stop()
			#}
			# if object doesn't exist, leave a log that this function should wait and will be discarded
			if (length(check_fn_done$Contents)==0){
				faasr_log(faasr, "faasr_abort_on_multiple_invocations: not the last trigger invoked - no flag")
				stop()
			}
		}

		# put random number into the file named "function.candidate"
		random_number <- sample(1:10000, 1)
		id_folder <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID)
		# Check whether directory exists, if not, create one.
		if (!dir.exists(id_folder)){dir.create(id_folder, recursive=TRUE)}
		file_names <- paste0(id_folder,"/",faasr$FunctionInvoke,".candidate")

		# Below is to avoid the race condition
		# acquire a Lock
		faasr_acquire(faasr)

		# if file named "function.candidate" exists, save it to the local
		#if (object_exists(file_names, log_server$Bucket)){
		#	save_object(file_names, file=file_names, bucket=log_server$Bucket)
		#}
		check_fn_candidate<-s3$list_objects_v2(Bucket=log_server$Bucket, Prefix=file_names)
		if (length(check_fn_candidate)!=0){
			if (file.exists(file_names)){file.remove(file_names)}
			s3$download_file(Key=file_names, Filename=file_names, Bucket=log_server$Bucket)
		}

		# append random number to the file / put it back to the s3 bucket
		write.table(random_number, file_names, col.names=FALSE, row.names = FALSE, append=TRUE, quote=FALSE)

		#put_object(file=file_names, object=file_names, bucket=log_server$Bucket)
		result<-s3$put_object(Body=file_names, Key=file_names, Bucket=log_server$Bucket)


		# save it to the local, again
		#save_object(file_names, file=file_names, bucket=log_server$Bucket)
		if (file.exists(file_names)){file.remove(file_names)}
		s3$download_file(Key=file_names, Filename=file_names, Bucket=log_server$Bucket)

		# release the Lock
		faasr_release(faasr)

		# if the first line of the file matches the random number, it will process codes behind it, else, it stops.
		if (as.character(random_number) == readLines(file_names,1)){
			NULL
			}else{
			cat('{\"msg\":\"faasr_abort_on_multiple_invocations: not the last trigger invoked - random number does not match\"}', "\n")
			stop()
			}

		}
}
