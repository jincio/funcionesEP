#' plotline
#'
#' @param data
#' @param categoria
#' @param var
#' @param grupo
#' @param titulo
#'
#' @return
#' @export
#'
#' @examples
linea <- function(data, categoria, var, grupo, titulo) {
  data %>%
    filter(cat == categoria) %>%
    mutate(fecha = as.Date(fecha)) %>%
    select(fecha, .data[[var]], .data[[grupo]]) %>%
    ggplot(aes(x = fecha,
               y = .data[[var]],
               col = .data[[grupo]])) +
    labs(x = "",
         y = "",
         col = "",
         subtitle = titulo) +
    geom_line(size = 0.8) +
    geom_point() +
    scale_colour_manual(values = c("#3EB595", "#506AD4", "#F2B138", "#8596A6", "#F2994B")) +
    theme_classic() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=rel(1),vjust=0.5,
                                       hjust=0.5,face="bold",
                                       color="gray48", lineheight=1.5))
}
