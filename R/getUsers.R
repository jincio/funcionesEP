#' getUsers
#'
#' @title Consulta Saldo: getUsers
#'
#' @description Get user variable by survey participants. It's linked with "dataClean" function
#' @param dataClean {"dataClean" function's output}
#'
#' @return
#' @export
#'
#' @examples
#'
getUsers = function(dataClean){
  usuarios=data %>%
    select(cel,usuario,email,id_encuesta,correo)%>%
    mutate(estado = c("Nuevo"))
  return(usuarios)
}
