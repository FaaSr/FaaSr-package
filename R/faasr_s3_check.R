faasr_s3_check <- function(faasr){

  for(server in names(faasr$DataStores){
    endpoint_check <- faasr$DataStores[[server]]$Endpoint
    region_check <- faasr$DataStores[[server]]$Region
    if (length(endpoint_check)==0 || endpoint_check=="") {
      faasr$DataStores[[server]]$Endpoint <- ""
    }else{
      if (!(startsWith(endpoint_check, "http"))){
        msg <- paste0('{\"faasr_start\":\"Invalid Logging server endpoint ',server,'\"}', "\n")
        cat(msg)
        stop()
      }
    }
  }
