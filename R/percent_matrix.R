#' percent_matrix
#'
#' @title percent_matrix
#'
#' @description Calculate percentages by questions with matrix style
#'
#' @param data        {The data to be displayed}
#' @param v1          {The first parameter in matrix}
#' @param v2          {The second parameter in matrix}
#' @param break_limit {number of characters to split the text}
#'
#' @usage percent_matrix(
#' data  = data,
#' v1    = data$variable1,
#' v2    = data$variable2,
#' break_limit = 5
#' )
#'
#' @return
#' @export
#'
#' @examples
percent_matrix=function(data,v1,v2,break_limit=45){
  Porcentaje=data %>%
    select(v1:v2) %>%
    gather(pregunta,respuesta) %>%
    group_by(pregunta,respuesta) %>%
    summarize(total = n()) %>%
    group_by(pregunta) %>%
    summarize(respuesta,total,TOT = sum(total)) %>%
    dplyr:: mutate(Porc = (total/TOT)*100,
                   Porc = substr(Porc,1,4),
                   Porc = as.numeric(Porc))%>%
    separate(pregunta, c('delete', 'pregunta'), sep=c("\\[|\\]")) %>%
    select(-delete) %>%
    mutate(pregunta=gsub("\\."," ",pregunta))%>%
    dplyr::filter(!is.na(respuesta))

  Porcentaje$pregunta=trimmer(Porcentaje$pregunta,break_limit = 45)

  return(Porcentaje)
}
