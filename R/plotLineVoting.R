#' plotlineVoting
#'
#' @title plotlineVoting
#'
#' @description A graphic to analyze time evolution of approval ratings
#'
#' @param data   {The data to be displayed in this layer}
#' @param vx     {Variable in the x-axis. Contains information about survey)
#' @param vy     {Variable in the y-axis. Contains information about responses}
#' @param grupo  {The grouping variable}
#' @param dates  {Date information}
#' @param titulo {The title of the layer}
#'
#' @return
#' @export
#'
#' @examples
#'
plotLineVoting=function(data,vx,vy,grupo,dates,titulo){

  grafico=ggplot(data, mapping = aes(x=vx,y=vy, group = grupo, color = grupo)) +
    geom_line() +
    scale_x_date(breaks = as.Date(dates),date_labels =  "%d %b %y") +
    geom_point( size=2, shape=21, fill="white")+
    labs(x="Encuesta", y="Respuesta")+
    scale_color_manual(values=c("#000066","red4","#339999"))+
    ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
    ggtitle(titulo)+
    theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",
                                    color="black",lineheight = 1.5))+
    theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
    theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
    labs(color="Respuesta")

  return(grafico)
}
