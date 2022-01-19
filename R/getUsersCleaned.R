#' getUsersCleaned
#'
#' @title Consulta Saldo: getUsersCleaned
#'
#' @description Get user variable by survey participants and delete duplicates. It's linked with "dataClean" function
#' @param dataClean {"dataClean" function's output}
#'
#' @return
#' @export
#'
#' @examples
#'
getUsersCleaned = function(dataLimpia){
  origin=import("usuariosTotal.xlsx") %>% mutate(estado = c("Registrado"))
  usuarios_new=getUsuarios(dataLimpia)

  #print(paste(nrow(nuevos_usuario),"nuevos usuarios"))
  usuariosTotal<-rbind(usuarios_new,origin)

  usuariosTotal=limpiezaData(usuariosTotal %>%
                               select(-usuario,-email)%>%
                               mutate(id_encuesta="total"),
                             email_column = 4 )

  usuariosTotal=usuariosTotal %>% select(cel, usuario, email, id_encuesta,correo, estado)

  rio::export(usuariosTotal,"usuariosTotal.xlsx",overwrite=TRUE)
  return(usuariosTotal)

}
