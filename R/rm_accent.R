#' rm_accent
#'
#' @title IDEA: rm_accent
#'
#' @description Removes all accents from characters in selected variable
#'
#' @param var {Variable. Type: character or factor}
#'
#' @usage rm_accent(var = data$variable)
#'
#' @return
#' @export
#'
#' @examples
#'
rm_accent <- function(var) {

  if(!is.character(var))
    var <- as.character(var)

  symbols <- c(acute = "áéíóúÁÉÍÓÚ")
  nudeSymbols <- c(acute = "aeiouAEIOU")
  accentTypes <- c("´")

  var=chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), var)

  for(i in which(accentTypes%in%var))
    var <- gsub(symbols[i],nudeSymbols[i], var)

  return(var)
}
