#' longtable
#'
#' @title longtable
#'
#' @description A table to integrate several analysis's output
#'
#' @param ...             {analysis}
#' @param float           {floating environment}
#' @param longtable.float {floating environment}
#' @param longtable.head  {head of final table}
#' @param filename        {exporting name}
#'
#' @usage plotLineVoting(
#' ...             = ...,
#' float           = TRUE,
#' longtable.float = FALSE,
#' longtable.head  = TRUE,
#' filename        = NULL
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
longtable.stargazer = function(..., float = T, longtable.float = F,
                               longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabulare environment for longtable
  res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  # cat(res, sep = "\n") <---- comment out this line
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ",
              collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}
