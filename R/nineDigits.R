#' nineDigits
#'
#' @title Consulta Saldo: nineDigits
#'
#' @description Select the nine last digits from cell phone variable
#'
#' @param x {cell phone variable}
#' @param n {digits to select}
#'
#' @usage nineDigits(
#' x = data$cellphone,
#' n = 9,
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
nineDigits = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
