#' standardVar
#'
#' @title IDEA: standardVar
#'
#' @description Removes all unusual characters (quotation marks & capital letters) in selected variable
#'
#' @param data {Database}
#' @param var  {Variable}
#' @param n    {Validates type of the "var" parameter. n == 1 if "var" is string and n > 1 if "var" is list}
#'
#' @usage standardVar(
#' data = data,
#' var  = data$variable,
#' n    = NULL,
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
standardVar=function(data,var,n=FALSE){
  ## Aqui lo que puedes hacer es validar si var == string o
  ## lista.. si es string es igual a n==1, si es lista n>1.
  ## Así evitas el parametro n.
  if(isFALSE(n)){ #falta corregir cuando es 1
    data<-data %>%
      mutate(var=gsub('","',";",var),
             var=gsub("[|,|]","",var),
             var=toupper(var),
             var=rm_accent(var))
  }
  else{
    data <- data %>%
      mutate_at(var,list(~gsub('","', ";", .))) %>%
      mutate_at(var,list(~gsub("[|]|,", "", .))) %>%
      mutate_at(var,toupper) %>%
      mutate_at(var,rm_accent)
  }

  return(data)
}
