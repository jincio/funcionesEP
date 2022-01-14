#' plotBarStacked
#'
#' @title plotBarStacked
#'
#' @description A graphic to visualize frequency of answers in a stacked format
#'
#' @param data   {The data to be displayed in this layer}
#' @param vx     {Variable in x axis. Type variable: character}
#' @param vy     {Variable in y axis. Contains the proportions from vx}
#' @param titulo {The title of the layer}
#' @param grupo  {The grouping variable}
#' @param flip   {The orientation of the layer}
#'
#' @usage plotBarStacked(
#' data      = survey,
#' vx        = survey$variable1,
#' vy        = survey$variable2,
#' titulo    = "Title of the graphic",
#' grupo     = survey$group,
#' filp      = FALSE
#' )
#'
#' @return
#' @export
#'
#' @examples
#'
plotBarStacked=function(data,vx,vy,titulo,grupo,flip=FALSE){
  if(isFALSE(flip)){
    grafico=data %>%
      ggplot(aes(x=vx, y=vy, fill= "Respuesta")) +
      geom_bar(stat="identity")+
      ggplot2::scale_fill_brewer(palette = "Set2")+
      labs(x="Pregunta", y="Porcentaje")+
      geom_text(aes(x =vx , y = vy, label = vy, group = grupo),
                position = position_stack(vjust = .5))+
      theme(axis.text.x = element_text(vjust = 0.5,hjust = 0.5))+
      ggtitle(titulo)+
      theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                       hjust=0.5,face="bold",
                                       color="black", lineheight=1.5))

    return(grafico)

  }else {
    grafico=data %>%
      ggplot(aes(x=vx, y=vy, fill = grupo)) +
      theme_bw()+
      scale_y_continuous(breaks = seq(0,100, by=10))+
      geom_bar(stat="identity") +
      labs(x="Pregunta",y ="Porcentaje", fill = "Respuesta")+
      geom_text(aes(x =vx , y = vy, label = vy, group = grupo),
                position = position_stack(vjust = .6),inherit.aes = T,fontface="bold", size=3)+
      coord_flip() +
      theme(axis.text.y = element_text(hjust = 1, size = 7, face = "bold")) +
      theme(axis.text.x = element_text(hjust = 0.5, size = 7, face = "bold"))+
      theme(legend.position = "bottom")+
      ggtitle(titulo)+
      theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                       hjust=0.5,face="bold",
                                       color="black", lineheight=1.5))

    return(grafico)
  }
}
