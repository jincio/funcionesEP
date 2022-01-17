#' dateSurvey
#'
#' @title dateSurvey
#'
#' @description A function to create surveys' date
#'
#' @param data  {The data to be displayed}
#' @param fecha {Date information about survey}
#'
#' @return
#' @export
#'
#' @examples
#'
dateSurvey = function(data,fecha){
  tiempo=data %>%
    mutate(tiempo=as.Date(fecha,format="%Y-%m-%d"))
  return(tiempo)
}
