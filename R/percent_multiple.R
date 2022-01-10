#' percent_multiple
#'
#' @title percent_multiple
#'
#' @description Calculate percentages by multiple answer questions
#'
#' @param data     {The data to be displayed}
#' @param variable {The variable to analyze}
#'
#' @usage percent_simple(
#' data      = data,
#' variable  = data$variable
#' )
#'
#' @return
#' @export
#'
#' @examples
percent_multiple=function(data,variable){
  Porcentaje=data %>%
    select(x=variable) %>%
    dplyr::rename(respuesta = x)%>%
    tidyr::separate(respuesta,into = c("1","2":"11"), sep = ", ") %>%
    tidyr::gather(columna,respuesta) %>%
    dplyr::filter(!is.na(respuesta))%>%
    dplyr::group_by(respuesta) %>%
    dplyr::summarize(total=n()) %>%
    dplyr::summarize(respuesta,total,TOT = sum(total)) %>%
    dplyr:: mutate(Porc = (total/TOT)*100,
                   Porc = as.numeric(Porc),
                   Porc=round(Porc,1))

  return(Porcentaje)
}
