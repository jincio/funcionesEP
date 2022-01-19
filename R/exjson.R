#' exjson
#'
#' @title Consulta Saldo: exjson
#'
#' @description Converts survey's data to a json format and saves it with the same name
#'
#' @param data {Output of the "dataFinal" function}
#' @param name {Name of exported file}
#'
#' @return
#' @export
#'
#' @examples
#'
exjson=function(data, name){
  rio::export(data,name,overwrite=TRUE)
}
