#' boxplot
#'
#' @title boxplot: A graphic to analyze data distribution
#'
#' @description A graphic to analyze distribution of some survey variable
#'
#'
#' @param data {The data to be displayed in this layer}
#' @param categoria {Ads category.There are two options:
#'                   If "general", data is conformed by only ads to general audience.
#'                   If "hombres", data is conformed by only ads to male audience.}
#' @param vary      {The variable to analyze, vertical axis (y)}
#' @param grupo     {The grouping variable}
#' @param titulo    {The title of the layer}
#'
#' @usage boxplot(
#' data      = survey,
#' categoria = "general",
#' vary      = "ads",
#' grupo     = "proyecto",
#' titulo    = "Title of the graphic"
#' )
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
