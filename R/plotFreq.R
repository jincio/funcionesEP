#' plotFreq
#'
#' @title IDEA: plotFreq
#'
#' @description Barplot about projects and motions by Congress of the Republic of Peru
#'
#' @param data     {Database to be displayed}
#' @param variable {A charactheristic about project or motion}
#' @param frec_min {Minimum frequency number to show in the plot}
#' @param titulo   {Title of the plot}
#'
#' @usage plotFreq(
#' data      = database,
#' variable  = "autor",
#' frec_min  = 1,
#' titulo    = "Title of the graphic"
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
plotFreq <- function(data, variable, frec_min=1, titulo){

  data %>%
    filter(campo == variable) %>%
    filter(!is.na(tipo)) %>%
    group_by(tipo) %>%
    count() %>%
    filter(n >= frec) %>%
    ggplot(aes(x = n,
               y = reorder(tipo, n))) +
    geom_col(show.legend = FALSE, fill = "#A9B6CC") +
    scale_fill_viridis_d() +
    labs(x = NULL,
         y = NULL,
         subtitle = titulo) +
    geom_text(aes(label = n),
              color = "black", hjust = -0.4, size = 5) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=rel(1.5),vjust=0.5,
                                       hjust=0.5,face="bold",
                                       color="gray48", lineheight=1.5))
}
