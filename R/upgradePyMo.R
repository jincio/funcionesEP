#' upgradePyMo
#'
#' @title IDEA: upgradePyMo
#'
#' @description Joins new projects and motions in "TablaClasificarFinal.xlsx" and shows the mark of them (new, registered, eliminated)
#'
#' @param proyectos {Database with law projects by Congress of the Republic of Peru}
#' @param mociones  {Database with political motions by Congress of the Republic of Peru}
#' @param upgrade   {Database to be upgrade, called "TablaClasificarFinal"}
#'
#' @return
#' @export
#'
#' @examples
#'
upgradePyMo <- function(proyectos, mociones, upgrade){

  # unir tablas de proyectos y mociones
  congreso <- bind_rows(proyectos, mociones)

  # seleccionar variables de interés
  congreso <- congreso %>% select(id,
                                  proponente,
                                  grupo_parlamentario,
                                  titulo,
                                  sumilla,
                                  votacion2)

  # etiquetar estado de proyectos y mociones
  casos_registrados  <- upgrade %>% mutate(estado = c("registrada"))

  casos_nuevos       <- anti_join(congreso, upgrade, by = "id") %>%
    mutate(estado = c("nueva"))

  casos_eliminados   <- anti_join(upgrade, congreso, by = "id") %>%
    mutate(estado = c("eliminada"))

  # unir data del congreso con upgrade
  upgrade_actualizado <- bind_rows(casos_nuevos,
                                     casos_eliminados,
                                     casos_registrados)

  # recodificar NA por 0
  upgrade_actualizado <- mutate_at(upgrade_actualizado,
                                     vars(votacion2, ind_ideolog:tipo_ejecutivo),
                                     ~replace(., is.na(.), 0))

  # Colocar variable "estado" al final
  upgrade_actualizado <- upgrade_actualizado %>%
    relocate(estado, .after = tipo_ejecutivo)

  return(upgrade_actualizado)

}
