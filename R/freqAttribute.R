#' freqAttribute
#'
#' @title Consulta Saldo: freqAttribute
#'
#' @description Proportion table by attribute's order
#'
#' @param data    {Survey data}
#' @param columns {Position of attributes}
#' @param rango   {Numbers of attributes}
#'
#' @return
#' @export
#'
#' @examples
#'
freqAttribute=function(data,columns,rango){
  col=columns
  tab1=data %>% select(all_of(col)) %>%
    slice(-1)
  tab1=unite(tab1, OrdenAtributos,c(rango),  sep = "-", remove = TRUE)
  prop_atr=as.data.frame(prop.table(table(tab1$OrdenAtributos)))
  names(prop_atr)=c("OrdenAtributos","Frecuencia")
  prop_atr=prop_atr%>%
    dplyr:: mutate(Porc =round(Frecuencia*100,3))

  return(prop_atr)
}
