#' percent_simple
#'
#' @title percent_simple
#'
#' @description Calculate percentages by answer simple questions
#'
#' @param data     {The data to be displayed}
#' @param variable {The variable to analyze}
#' @param vgrupo   {The grouping variable}
#' @param vreg     {Specifying region}
#'
#' @usage percent_simple(
#' data      = data,
#' variable  = data$variable
#' vgrupo    = NULL
#' vreg      = NULL
#' )
#'
#' @return
#' @export
#'
#' @examples

percent_simple = function(data,variable,vgrupo=NULL,vreg=NULL){

  if(is.null(vgrupo)){
    if(is.null(vreg)){
      Porcentaje = data%>%
        dplyr::rename(respuesta = variable)%>%
        dplyr::filter(!is.na(respuesta)) %>%
        dplyr::group_by(respuesta)%>%
        dplyr::summarize(total=n())%>%
        dplyr::summarize(respuesta,total,TOT = sum(total))%>%
        dplyr:: mutate(Porc = (total/TOT)*100,
                       Porc = as.numeric(Porc),
                       Porc = round(Porc,1))

      return(Porcentaje)

    }else {
      Porcentaje = data%>%
        dplyr::rename(respuesta=variable) %>%
        dplyr::mutate(data,Region = ifelse(vreg == "Lima","Lima","No Lima"))%>%
        dplyr::group_by(respuesta,Region)%>%
        summarize(total=n())%>%
        group_by(Region)%>%
        summarize(respuesta,total,TOT = sum(total))%>%
        dplyr:: mutate(Porc = (total/TOT)*100,
                       Porc = as.numeric(Porc),
                       Porc = round(Porc,1))

      return(Porcentaje)
    }
  } else {
    if(is.null(vreg)){
      Porcentaje = data%>%
        dplyr::rename(respuesta = variable,
                      sexo = vgrupo)%>%
        dplyr::group_by(respuesta,sexo)%>%
        summarize(total=n())%>%
        group_by(sexo)%>%
        summarize(respuesta,total,TOT = sum(total))%>%
        dplyr:: mutate(Porc = (total/TOT)*100,
                       Porc = as.numeric(Porc),
                       Porc = round(Porc,1))

      return(Porcentaje)
    }
  }
}
