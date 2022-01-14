#' plotBarVoting
#'
#' @title plotBarVoting
#'
#' @description A graphic to visualize voting intention. Shows a barplot with names by candidates and political parties
#'
#' @param data   {The voting data to be displayed in this layer}
#' @param vx     {Variable in x axis}
#' @param vy     {Variable in y axis}
#' @param titulo {The title of the layer}
#'
#' @usage plotBarVoting(
#' data      = survey,
#' vx        = survey$variable1,
#' vy        = survey$variable2,
#' titulo    = "Title of the graphic",
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
plotBarVoting=function(data,vx,vy=Porc,titulo){
  grafico=data %>%
    ggplot2::ggplot(aes(x=reorder(vx,-vy), y=vy)) +
    ggplot2::geom_bar(stat="identity", position = position_dodge(),
                      color ="#424632", fill="#424632") +
    ggplot2::ylim(0,70)+
    ggplot2::labs(x = "Respuesta", y = "Porcentaje") +
    ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5)+
    ggplot2::theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    ggtitle(titulo)+
    theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                     hjust=0.5,face="bold",
                                     color="black", lineheight=1.5))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  return(grafico)
}
