#' boxplot
#'
#' @param data
#' @param categoria
#' @param vary
#' @param grupo
#' @param titulo
#'
#' @return
#' @export
#'
#' @examples
boxplot <- function(data, categoria, vary, grupo, titulo) {
  data %>%
    filter(cat == categoria) %>%
    ggplot(aes(y = .data[[vary]])) +
    geom_boxplot(aes(fill = .data[[grupo]])) +
    scale_fill_manual(values = c("#3EB595", "#506AD4", "#F2B138", "#8596A6", "#F2994B")) +
    labs(x = "",
         y = "",
         fill = "",
         subtitle = titulo) +
    theme_classic() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.subtitle = element_text(size=rel(1),vjust=0.5,
                                       hjust=0.5,face="bold",
                                       color="gray48", lineheight=1.5))
}
