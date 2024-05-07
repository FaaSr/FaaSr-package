docker_default_version <- "latest"
docker_default_image <- "faasr/local-test"

#' @name faasr_test
#' @title faasr_test
#' @description 
#' Client tools for local tests
#' Users can use local test by using both for Docker and local file system
#' @param use_docker a list of docker configuration - use, version and image.
#' @import cli
#' @return return nothing / executes the FaaS
#' @export 
#' @examples
#' if (interactive()){
#' test <- faasr("test.json", "env")
#' test$faasr_test()
#' test$faasr_test(use_docker=list(use=TRUE, version="test", image="docker.io/test/test-docker:image"))
#' }

faasr_test <- function(use_docker=
                        list(
                          use=FALSE, 
                          version=docker_default_version, 
                          image=docker_default_image
                        )
                      ){
  # Get configurations
  svc <- .faasr_get_svc()
  faasr_wd <- svc$wd
  if (!dir.exists(faasr_wd)){
    faasr_wd <- getwd()
  }
  faasr <- svc$json
  cred <- faasr_collect_sys_env(faasr,svc$cred)
  faasr <- faasr_replace_values(faasr, cred)
  docker_image <- paste0(use_docker$image, ":", use_docker$version)

  # if no "faasr_data" directory created by 'faasr' function, no execution
  if (!dir.exists("faasr_data")){
    cli_alert_danger("faasr_data directory no found")
    return("")
  }
  on.exit(setwd(faasr_wd))
  setwd("faasr_data")
  faasr_data_wd <- getwd()

  # if there's "temp" folder, remove it, and create new "temp" folder.
  if (dir.exists("temp")){
    unlink("temp", recursive=TRUE)
  }
  dir.create("temp/faasr_state_info", recursive=TRUE)
  cli_alert_success("Create test temporary folders")

  # Download schema from github
  utils::download.file("https://raw.githubusercontent.com/FaaSr/FaaSr-package/main/schema/FaaSr.schema.json", "temp/FaaSr.schema.json")
  
  # start the test by calling 'faasr_test_start'
  on.exit(setwd(faasr_wd))
  setwd("temp")
  result <- faasr_test_start(faasr, faasr_data_wd, use_docker$use, docker_image)

  # if result==TRUE, it is successful. Else, failed with "result" data.
  if (result == TRUE){
    on.exit(setwd(faasr_wd))
    setwd(faasr_wd)
    cli_alert_success("Function execution successfully")
  } else {
    on.exit(setwd(faasr_wd))
    setwd(faasr_wd)
    cli_alert_danger(result)
    cli_alert_info("Wrong result: test close")
  }
}
.faasr_user$operations$faasr_test <- faasr_test


faasr_test_start <- function(faasr, faasr_data_wd, docker_use, docker_image){
  # function for clearing the console
  clc <- function() cat(rep("\n", 30))

  # get configuration
  current_func <- faasr$FunctionInvoke
  cli::cli_h1("")
  faasr_input <- jsonlite::toJSON(faasr, auto_unbox=TRUE)

  # run the test - configuration/user_function tests
  # if docker_use == TRUE, run the docker image / if docker_use !=TRUE, run the local function.
  if (docker_use){
    result <- system(paste0("docker run --rm --name faasr-",current_func,
                    " --mount type=bind,source='",faasr_data_wd,"',target='/faasr_data' ",
                    docker_image," '", faasr_input,"'"), 
                    intern=TRUE, ignore.stderr = FALSE, ignore.stdout= FALSE)
  } else {
    result <- faasr_test_run(faasr)
  }

  # if result != TRUE, it means there's an error. return error message.
  if (result[length(result)] != TRUE && result[length(result)] != "TRUE" ){
    return(result[length(result)])
  }

  # if there's no next function, return TRUE.
  next_funcs <- faasr$FunctionList[[faasr$FunctionInvoke]]$InvokeNext
  if (is.null(next_funcs)){
    return(TRUE)
  }

  cli_alert_info(paste0("Testing successful - ",current_func))

  # recursively run the code by calling another "faasr_test_start"
  for (next_func in next_funcs){
    faasr$FunctionInvoke <- next_func
    faasr_wd <- getwd()
    on.exit(setwd(faasr_wd))
    setwd("..")
    cli_alert_info("Trigger Next functions")
    result <- faasr_test_start(faasr, faasr_data_wd, docker_use, docker_image)
    if (result[length(result)] != TRUE){
      return(result[length(result)])
    }
  }
  return(TRUE)
}

# faasr_test_run - running the same procedure of FaaSr
faasr_test_run <- function(faasr, docker_use=FALSE){

  # Read the R scripts, which are given by users
  if (docker_use){
    for (i in list.files("/faasr_data/R")){
      source(paste0("/faasr_data/R/",i))
    }
  } else {
    for (i in list.files("../R")){
      source(paste0("../R/",i))
    }
  }
  
  cli_alert_success("Read R files")

  current_func <- faasr$FunctionInvoke
  func_name <- faasr$FunctionList[[faasr$FunctionInvoke]]$FunctionName

  # create the 'FunctionInvoke' directory.
  cli::cli_h2(paste0("Start testing: ",current_func))
  if (!dir.exists(current_func)){
    dir.create(current_func)
  }
  faasr_wd <- getwd()
  on.exit(setwd(faasr_wd))
  setwd(current_func)

  # Start configuration check - this is same procedure to 'faasr_start'
  result <- faasr_configuration_check(faasr, docker_use)
  if (result != TRUE){
    if (result == "next"){
      return(TRUE)
    } else{
      cli_alert_danger(result)
      return("")
    }
  }
  cli_alert_success("Configuration checked")

  # Download and install dependencies
  check <- faasr_dependency_install(faasr, func_name)
  if (check != TRUE){
    cli_alert_danger(check)
    return("")
  }
  cli_alert_success("Dependencies installed")

  # Run the user function. 
  # If it is successful, it returns TRUE, if not, it returns "" and prints the error message.
  result <- faasr_user_function_check(faasr, docker_use)
  if (result != TRUE){
    cli_alert_danger(result)
    return("")
  }
  cli_alert_success("User function checked")
  return(TRUE)
}

# faasr_user_function_check - check the user function.
faasr_user_function_check <- function(faasr, docker_use=FALSE){

  # get the user function
  faasr_wd <- getwd()
  func_name <- faasr$FunctionList[[faasr$FunctionInvoke]]$FunctionName
  user_function <- try(get(func_name), silent=TRUE)

  # if 'try' function creates the error, return the error.
  if (methods::is(user_function, "try-error")){
    return(paste0("Can't find User function ",func_name))
  }

  # change the user function with local test - local tests don't use FaaS or S3 storage
  # Parse the user function and check the FaaSr functions.
  # Replace them with other local test functions.
  user_call <- trimws(deparse(user_function))
  if (docker_use){
    for (i in 1:length(user_call)){
      user_call[i] <- gsub("FaaSr::faasr_put_file", "faasr_docker_local_test_put_file", user_call[i])
      user_call[i] <- gsub("faasr_put_file", "faasr_docker_local_test_put_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_get_file", "faasr_docker_local_test_get_file", user_call[i])
      user_call[i] <- gsub("faasr_get_file", "faasr_docker_local_test_get_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_delete_file", "faasr_docker_local_test_delete_file", user_call[i])
      user_call[i] <- gsub("faasr_delete_file", "faasr_docker_local_test_delete_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_log", "faasr_local_test_log", user_call[i])
      user_call[i] <- gsub("faasr_log", "faasr_local_test_log", user_call[i])   
    }
  } else {
    for (i in 1:length(user_call)){
      user_call[i] <- gsub("FaaSr::faasr_put_file", "faasr_local_test_put_file", user_call[i])
      user_call[i] <- gsub("faasr_put_file", "faasr_local_test_put_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_get_file", "faasr_local_test_get_file", user_call[i])
      user_call[i] <- gsub("faasr_get_file", "faasr_local_test_get_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_delete_file", "faasr_local_test_delete_file", user_call[i])
      user_call[i] <- gsub("faasr_delete_file", "faasr_local_test_delete_file", user_call[i])

      user_call[i] <- gsub("FaaSr::faasr_log", "faasr_local_test_log", user_call[i])
      user_call[i] <- gsub("faasr_log", "faasr_local_test_log", user_call[i])      
    }   
  }
  user_function <- eval(parse(text=user_call))

  user_args <- faasr_get_user_function_args(faasr) 

  # Execute the user function.
  faasr_result <- tryCatch({expr=do.call(user_function, user_args)}, error=function(e){e})
  on.exit(setwd(faasr_wd))
  setwd(faasr_wd)
    
  # If there's error, return the message
  if (methods::is(faasr_result, "error")){
    check <- FALSE
    err_code <- deparse(faasr_result$call)

    for (i in 1:length(user_call)){
      if (err_code[1] == user_call[i]){
        check <- TRUE
        return(paste0("Line", i-1, " : ", faasr_result$message))
      }
    }   
    if (!check){
      return(faasr_result$message)
    }
  } else {
    if (docker_use){
      write.table("TRUE", file=paste0("/faasr_data/temp/faasr_state_info/", faasr$FunctionInvoke, ".done"), row.names=F, col.names=F)
    } else {
      write.table("TRUE", file=paste0("../faasr_state_info/", faasr$FunctionInvoke, ".done"), row.names=F, col.names=F)
    }
    return(TRUE)
  }
}

# check the configuration check - this is same procedure to 'faasr_start'
faasr_configuration_check <- function(faasr, docker_use=FALSE){
  
  # copy the schema
  if (docker_use){
    file.copy(from="/faasr_data/temp/FaaSr.schema.json", to="FaaSr.schema.json")
  } else {
    file.copy(from="../FaaSr.schema.json", to="FaaSr.schema.json")
  }
  
  # check the schema
  faasr <- try(faasr_parse(toJSON(faasr,auto_unbox=TRUE)), silent=TRUE)
  if (methods::is(faasr, "try-error")){
    # schema errors
    return(attr(faasr, "condition"))
  } 

  # check the data storage configuration
  for (datastore in names(faasr$DataStores)){
    endpoint <- faasr$DataStores[[datastore]]$Endpoint
    if ((!is.null(endpoint)) && (endpoint != "") && !startsWith(endpoint, "https")){
      # data store errors
      return("data store errors")
    }
  }

  log_server_name <- faasr$DefaultDataStore
  if (!log_server_name %in% names(faasr$DataStores)){
    # default server errors
    return("default server errors")
  }

  if (!is.null(faasr$LoggingDataStore)){
    log_server_name <- faasr$LoggingDataStore
    if (!log_server_name %in% names(faasr$DataStores)){
      # logging server errors
      return("logging server errors")
    }
  }

  # check the workflow cycle.
  pre <- try(faasr_check_workflow_cycle(faasr), silent=TRUE)
  if (methods::is(pre, "try-error")){
    # cycle/unreachable faasr_state_info errors
    return(attr(pre, "condition"))
  }
  
  # if the number of predecessor nodes is more than 2, skip running.
  if (length(pre)>1){
    for (func in pre) {
      func_done <- paste0(func,".done")
      if (docker_use){
        check_fn_done_list <- list.files("/faasr_data/temp/faasr_state_info")
      } else {
        check_fn_done_list <- list.files("../faasr_state_info")
      }
      if (!func_done %in% check_fn_done_list){
        return("next")
      }
    }
  }

  return(TRUE)
}


# local test function for faasr_put_file
faasr_local_test_put_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
   
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("../../files/", remote_folder)
  put_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  local_folder <- paste0("./", local_folder)
  put_file <- paste0(local_folder,"/",local_file)
  
  if (!dir.exists(local_folder)){
    dir.create(local_folder, recursive=TRUE)
  }

  if (!dir.exists(remote_folder)){
    dir.create(remote_folder, recursive=TRUE)
  }  

  file.copy(from=put_file, to=put_file_s3)

}

# local test function for faasr_get_file
faasr_local_test_get_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("../../files/", remote_folder)
  get_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  local_folder <- paste0("./", local_folder)
  get_file <- paste0(local_folder,"/",local_file)
  
  if (!dir.exists(local_folder)){
    dir.create(local_folder, recursive=TRUE)
  }

  if (!dir.exists(remote_folder)){
    dir.create(remote_folder, recursive=TRUE)
  }  

  file.copy(to=get_file, from=get_file_s3)

}

# local test function for faasr_delete_file
faasr_local_test_delete_file <- function(server_name=NULL, remote_folder="", remote_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("../../files/", remote_folder)
  delete_file_s3 <- paste0(remote_folder, "/", remote_file)

  unlink(delete_file_s3, recursive=TRUE)

}

# local test function for faasr_log
faasr_local_test_log <- function(log_message){
  # TBD
}

# local(docker) test function for faasr_put_file
faasr_docker_local_test_put_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
   
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("/faasr_data/files/", remote_folder)
  put_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  local_folder <- paste0("./", local_folder)
  put_file <- paste0(local_folder,"/",local_file)
  
  if (!dir.exists(local_folder)){
    dir.create(local_folder, recursive=TRUE)
  }

  if (!dir.exists(remote_folder)){
    dir.create(remote_folder, recursive=TRUE)
  }  

  file.copy(from=put_file, to=put_file_s3)

}

# local(docker) test function for faasr_get_file
faasr_docker_local_test_get_file <- function(server_name=NULL, remote_folder="", remote_file, local_folder=".", local_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("/faasr_data/files/", remote_folder)
  get_file_s3 <- paste0(remote_folder, "/", remote_file)

  local_folder <- sub("^/+", "", sub("/+$", "", local_folder))
  local_file <- sub("^/+", "", sub("/+$", "", local_file))
  local_folder <- paste0("./", local_folder)
  get_file <- paste0(local_folder,"/",local_file)
  
  if (!dir.exists(local_folder)){
    dir.create(local_folder, recursive=TRUE)
  }

  if (!dir.exists(remote_folder)){
    dir.create(remote_folder, recursive=TRUE)
  }  

  file.copy(to=get_file, from=get_file_s3)

}

# local(docker) test function for faasr_delete_file
faasr_docker_local_test_delete_file <- function(server_name=NULL, remote_folder="", remote_file){
  
  remote_folder <- sub("^/+", "", sub("/+$", "", remote_folder))
  remote_file <- sub("^/+", "", sub("/+$", "", remote_file))
  remote_folder <- paste0("/faasr_data/files/", remote_folder)
  delete_file_s3 <- paste0(remote_folder, "/", remote_file)

  unlink(delete_file_s3, recursive=TRUE)

}

# installing dependencies
faasr_dependency_install <- function(faasr, funcname, new_lib=NULL){

  # install CRAN packages
  packages <- faasr$FunctionCRANPackage[[funcname]]
  faasr_install_cran(packages, lib_path=new_lib)

  # install Git packages
  ghpackages <- faasr$FunctionGitHubPackage[[funcname]]
  faasr_install_git_package(ghpackages, lib_path=new_lib)

  return(TRUE)
}

# Sub function for faasr_dependency_install
faasr_install_cran <- function(packages, lib_path=NULL){
  if (length(packages)==0){
  } else{
    for (package in packages){
	    utils::install.packages(package, lib=lib_path)
	  }
  }
}

# Sub function for faasr_dependency_install
# function to help install "git packages"
faasr_install_git_package <- function(ghpackages, lib_path=NULL){
  if (length(ghpackages)==0){
  } else{
    for (ghpackage in ghpackages){
	    withr::with_libpaths(new=lib_path, devtools::install_github(ghpackage, force=TRUE))
	  }
  }
}

