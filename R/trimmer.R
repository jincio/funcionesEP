#' trimmer
#'
#' @title trimmer
#'
#' @description Adds '\n' after an group of specified characthers
#'
#' @param x           {column or variable}
#' @param break_limit {number of characters to split the text}
#'
#' @usage trimmer(
#' x            = data$column,
#' break_limit  = 10
#' )
#'
#' @return
#' @export
#'
#' @examples
trimmer <- function(x,break_limit){
  sapply(strwrap(x, break_limit, simplify=FALSE), paste, collapse="\n")
}
