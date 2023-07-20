#' @title Implements functions to support locking primitives
#' @description Uses locking algorithm with S3 to enforce single User Function execution when there are multiple predecessors
#' TBD: describe each function
#' @param faasr list with parsed and validated Payload

library("paws")

# Read-Set Memory implementation
faasr_rsm <- function(faasr) {
  # Set env for flag and lock
  flag_content <- as.character(sample(1:1000,1))
  flag_path <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"/flag/")
  flag_name <- paste0(flag_path,flag_content)
  lock_name <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")

  # Set env for the storage.
  target_s3 <- faasr$LoggingServer
  target_s3 <- faasr$DataStores[[target_s3]]
  #Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region, "AWS_SESSION_TOKEN" = "")
  s3<-paws::s3(
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

  # Make a loop
  while(TRUE) {
    # Put a object named "functionname/flag" with the content "T" into the S3 bucket
	result <- s3$put_object(Key=flag_name, Bucket=target_s3$Bucket)

	# if someone has a flag i.e.,faasr_anyone_else_interested returns TRUE, delete_flag and try again.
	if(faasr_anyone_else_interested(faasr, target_s3, flag_path, flag_name)) {
	  s3$delete_object(Key=flag_name, Bucket=target_s3$Bucket)
	# if nobody has a flag i.e.,faasr_anyone_else_interested returns FALSE, check the lock condition.
	} else {
	# if ".lock" exists in the bucket, return FALSE, and try all over again.
	  check_lock <- s3$list_objects_v2(Prefix=lock_name, Bucket=target_s3$Bucket)
	  if (length(check_lock$Contents) != 0) {
	    return(FALSE)
	  # if ".lock" does not exist, make a new lock with the content of flag_content
	  } else {
	    writeLines(flag_content, "lock.txt")
		result <- s3$put_object(Body="lock.txt", Key=lock_name, Bucket=target_s3$Bucket)
		file.remove("lock.txt")

		# release the flag and get out of the while loop
		 #delete_object(flag_name, target_s3$Bucket)
		s3$delete_object(Key=flag_name, Bucket=target_s3$Bucket)
		return(TRUE)
	  }
	}

  }
}

# lock implementation - acquire
faasr_acquire<-function(faasr) {
	# Call faasr_rsm to get a lock, faasr_rsm returns either TRUE or FALSE
	Lock <- faasr_rsm(faasr)

	#if function acquires a lock, it gets out of the loop
	while(TRUE) {
		# if Lock is TRUE i.e., this function has a lock, return TRUE i.e., get out of the While loop
		if (Lock) {
		  return(TRUE)
		} else {
		# if it doesn't, keep trying to get the flag&lock by calling faasr_rsm again until it returns TRUE.
		  Lock <- faasr_rsm(faasr)
		}
	}
}


# lock implementation - release
faasr_release<-function(faasr) {
	# Set env for locks.
	lock_name <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")
	target_s3 <- faasr$LoggingServer
	target_s3 <- faasr$DataStores[[target_s3]]
	#Sys.setenv("AWS_ACCESS_KEY_ID"=target_s3$AccessKey, "AWS_SECRET_ACCESS_KEY"=target_s3$SecretKey, "AWS_DEFAULT_REGION"=target_s3$Region, "AWS_SESSION_TOKEN" = "")
	s3<-paws::s3(
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
	# delete the file named ".lock"
	#delete_object(lock_name, target_s3$Bucket)
	s3$delete_object(Key=lock_name, Bucket=target_s3$Bucket)
}


# Anyone_else_interested implementation
faasr_anyone_else_interested <- function(faasr, target_s3, flag_path, flag_name){
        # get_bucket_df function may have Compatibility problem for some region (us-east-2, ca-central-1.., working in these regions may have error )
	# which the bucket object "Owner" part does not have "DisplayName", just have "ID" value.
	# alternative package: may use "paws" library list_objects_v2 function
	# pool is a list of flag names
	#pool <- get_bucket_df(target_s3$Bucket,prefix=flag_path)
	check_pool <- s3$list_objects_v2(Bucket=target_s3$Bucket, Prefix=flag_path)
	pool <- lapply(check_pool$Contents, function(x) x$Key)

	# if this function sets the flag and it is the only flag, return FALSE, if not, return TRUE
	if (flag_name %in% pool && length(pool) == 1) {
		return(FALSE)
	} else {
		return(TRUE)
	}
}
