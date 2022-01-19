#' statsTime
#'
#' @title Consulta Saldo: statsTime
#'
#' @description Shows a table with descriptive statistics by  sections of the poll
#'
#' @param data {Output of the "timeSection" function}
#'
#' @return
#' @export
#'
#' @examples
#'
statsTime = function(data){
  tablaTiempo=data  %>%
    group_by(preguntas,bloque) %>%
    summarise(PromedioTiempo=mean(tiempo),
              Minimo=min(tiempo),
              Maximo=max(tiempo),
              DE=sd(tiempo),
              Mediana=median(tiempo),
              Obs=n()) %>%
    arrange(bloque)

  return(tablaTiempo)
}
