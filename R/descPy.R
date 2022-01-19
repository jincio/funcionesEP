#' descPy
#'
#' @title IDEA: descPy
#'
#' @description Descriptive table about the projects by Congress of the Republic of Peru. Projects are divided into principal author, last state and parliamentary group
#'
#' @param proyectos {Database with law projects by Congress of the Republic of Peru}
#'
#' @return
#' @export
#'
#' @examples
#'
descPy <- function(proyectos){

  proyectos <- proyectos %>%
    mutate(autor = rm_accent(autor),
           grupo_parlamentario = rm_accent(grupo_parlamentario)) %>%
    mutate_at(c("proponente", "grupo_parlamentario"), toupper)

  bancada <- proyectos %>%
    filter(proponente == "CONGRESO") %>%
    filter(autor != is.na(autor)) %>%
    group_by(fecha = presentacion_fecha,
             tipo = grupo_parlamentario) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("bancada")) %>%
    arrange(fecha)

  autor <- proyectos %>%
    filter(proponente == "CONGRESO") %>%
    filter(autor != is.na(autor)) %>%
    group_by(fecha = presentacion_fecha,
             tipo = autor) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("autor")) %>%
    arrange(fecha)

  estado <- proyectos %>%
    filter(proponente == "CONGRESO") %>%
    filter(autor != is.na(autor)) %>%
    group_by(fecha = presentacion_fecha,
             tipo = ultimo_estado) %>%
    count(sort = TRUE, name = "valor") %>%
    mutate(campo = c("estado")) %>%
    arrange(fecha)

  data <- rbind(estado, autor, bancada) %>%
    relocate(campo, .after = fecha) %>%
    mutate(tipo = case_when(
      tipo == "Publicado en el Diario Oficial El Peruano" ~ "Publicado",TRUE ~ tipo))

  return(data)

}
