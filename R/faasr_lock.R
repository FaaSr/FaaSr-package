#' @name faasr_lock
#' @title faasr_rsm
#' @description 
#' Uses locking algorithm with S3 to enforce single User Function execution when there are multiple predecessors
#' @import paws
#' @param faasr list with parsed and validated Payload

library("paws")

# Read-Set Memory implementation
faasr_rsm <- function(faasr) {
  # Set env for flag and lock
  flag_content <- as.character(sample(1:10000000,1))
  flag_path <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"/flag/")
  flag_name <- paste0(flag_path,flag_content)
  lock_name <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")

  # Set env for the storage.
  if (is.null(faasr$LoggingDataStore)){
    target_s3 <- faasr$DefaultDataStore
  } else {
    target_s3 <- faasr$LoggingDataStore
  }
  target_s3 <- faasr$DataStores[[target_s3]]
  s3<-paws::s3(
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
  cnt <- 0
  max_cnt <- 4

  # Make a loop
  while(TRUE) {
    # Put a object named "functionname/flag" with the content "T" into the S3 bucket
	result <- s3$put_object(Key=flag_name, Bucket=target_s3$Bucket)
	# if someone has a flag i.e.,faasr_anyone_else_interested returns TRUE, delete_flag and try again.
	if(faasr_anyone_else_interested(faasr, target_s3, flag_path, flag_name)) {
	  s3$delete_object(Key=flag_name, Bucket=target_s3$Bucket)
	  if (cnt > max_cnt){
	    Sys.sleep(2^max_cnt)
	  } else {
	    Sys.sleep(2^cnt)
	    cnt <- cnt+1
	  }
		
	# if nobody has a flag i.e.,faasr_anyone_else_interested returns FALSE, check the lock condition.
	} else {
	# if ".lock" exists in the bucket, return FALSE, and try all over again.
	  check_lock <- s3$list_objects_v2(Prefix=lock_name, Bucket=target_s3$Bucket)
	  if (length(check_lock$Contents) == 0) {
	  # if ".lock" does not exist, make a new lock with the content of flag_content
		writeLines(flag_content, "lock.txt")
		result <- s3$put_object(Body="lock.txt", Key=lock_name, Bucket=target_s3$Bucket)
		file.remove("lock.txt")
		# release the flag and get out of the while loop
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
	cnt <- 0
	max_cnt <- 4
	# if function acquires a lock, it gets out of the loop
	while(TRUE) {
		# if Lock is TRUE i.e., this function has a lock, return TRUE i.e., get out of the While loop
		if (Lock) {
		  return(TRUE)
		} else {
		# if it doesn't, keep trying to get the flag&lock by calling faasr_rsm again until it returns TRUE.
		# before retrying, sleep exponential to cnt
		  if (cnt > max_cnt){
		    Sys.sleep(2^max_cnt)
		  } else {
		    Sys.sleep(2^cnt)
		    cnt <- cnt+1
		  }
		  Lock <- faasr_rsm(faasr)
		}
	}
}


# lock implementation - release
faasr_release<-function(faasr) {
	# Set env for locks.
	lock_name <- paste0(faasr$FaaSrLog,"/", faasr$InvocationID,"/",faasr$FunctionInvoke,"./lock")
	if (is.null(faasr$LoggingDataStore)){
          target_s3 <- faasr$DefaultDataStore
        } else {
          target_s3 <- faasr$LoggingDataStore
        }

	target_s3 <- faasr$DataStores[[target_s3]]
	s3<-paws::s3(
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
	# delete the file named ".lock"
	s3$delete_object(Key=lock_name, Bucket=target_s3$Bucket)
}


# Anyone_else_interested implementation
faasr_anyone_else_interested <- function(faasr, target_s3, flag_path, flag_name){

    s3<-paws::s3(
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

	# pool is a list of flag names
	check_pool <- s3$list_objects_v2(Bucket=target_s3$Bucket, Prefix=flag_path)
	pool <- lapply(check_pool$Contents, function(x) x$Key)

	# if this function sets the flag and it is the only flag, return FALSE, if not, return TRUE
	if (flag_name %in% pool && length(pool) == 1) {
		return(FALSE)
	} else {
		return(TRUE)
	}
}
