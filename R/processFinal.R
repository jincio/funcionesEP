#' processFinal
#'
#' @title Consulta Saldo: processFinal
#'
#' @description Joins processed tables of previous polls with the recent one
#'
#' @param data     {Data survey}
#' @param ganador  {User of survey winner}
#' @param fecha    {Date information about survey}
#' @param encuesta {Recent survey}
#'
#' @return
#' @export
#'
#' @examples
#'
processFinal=function(data,ganador,fecha,encuesta){
  dataProcesada_origin=import("dataProcesada.xlsx")
  dataProcesada_origin=dataProcesada_origin %>% filter(id_encuesta != encuesta)
  dataProcesada_new=processData(data,ganador,fecha)
  dataProcesada=rbind(dataProcesada_origin,dataProcesada_new)

  rio::export(dataProcesada,"dataProcesada.xlsx",overwrite=TRUE)
  return(dataProcesada)
}
