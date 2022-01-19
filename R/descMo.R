#' descMo
#'
#' @title IDEA: descMo
#'
#' @description Descriptive table about the motions by Congress of the Republic of Peru. Motions are divided into principal author, type, last state and parliamentary group
#'
#' @param proyectos {Database with motions by Congress of the Republic of Peru}
#'
#' @return
#' @export
#'
#' @examples
#'
descMo <- function(mociones){

  mociones <- mociones %>%
    left_join(import("./Output/TablaCongresistasActivos.xlsx") %>%
                select(congresista, partido, bancada) %>%
                rename(autor = congresista),
              by = "autor") %>%
    mutate(partido = ifelse(autor == "HERRERA MAMANI FERNANDO MARIO","PARTIDO POLITICO NACIONAL PERU LIBRE", partido),
           bancada = ifelse(autor == "HERRERA MAMANI FERNANDO MARIO","PARTIDO POLITICO NACIONAL PERU LIBRE", bancada))

  partido <- mociones %>%
    group_by(fecha = presentacion_fecha,
             tipo = partido) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("partido")) %>%
    arrange(fecha)

  autor <- mociones %>%
    group_by(fecha = presentacion_fecha,
             tipo = autor) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("autor")) %>%
    arrange(fecha)

  estado <- mociones %>%
    group_by(fecha = presentacion_fecha,
             tipo = estado_mocion_fin) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("estado")) %>%
    arrange(fecha)

  tipo <- mociones %>%
    group_by(fecha = presentacion_fecha,
             tipo = tipo_mocion) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("tipo")) %>%
    arrange(fecha)

  data <- rbind(estado, tipo, autor, partido) %>%
    relocate(campo, .after = fecha)

  return(data)

}
