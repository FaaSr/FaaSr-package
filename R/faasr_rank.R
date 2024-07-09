#' @name faasr_rank
#' @title faasr_rank
#' @description 
#' Helper function to let users identify the rank and max rank in the function
#' This function is used by users in the container.
#' @return return a list of Rank & MaxRank
#' @export
#' @examples
#' # This function can be run only in the container
#' if (interactive()){
#' rank_info <- faasr_rank()
#' MaxRank <- rank_info$MaxRank 
#' Rank <- rank_info$Rank
#' }
#' 

faasr_rank <- function(){
  current_func <- .faasr$FunctionInvoke
  if (length(.faasr$FunctionList[[current_func]]$Rank) != 0){
    parts <- unlist(strsplit(.faasr$FunctionList[[current_func]]$Rank, "[/]"))
    return(list(MaxRank=parts[2], Rank=parts[1]))
  } else {
    return(list())
  }
}