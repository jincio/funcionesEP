#' plotBar
#'
#' @title plotBar
#'
#' @description A graphic to visualize frequency of answers
#'
#' @param data   {The data to be displayed in this layer}
#' @param vx     {Variable in x axis}
#' @param vy     {Variable in y axis}
#' @param titulo {The title of the layer}
#' @param grupo  {The grouping variable}
#' @param flip   {The orientation of the layer}
#'
#' @return
#' @export
#'
#' @examples
#'
plotBar=function(data,vx,vy=Porc,titulo,grupo=NULL,flip=FALSE){

  if(is.null(grupo)){
    if(isFALSE(flip)){
      grafico=data %>%
        ggplot2::ggplot(aes(x=reorder(vx,-vy), y=vy)) +
        ggplot2::geom_bar(stat="identity",
                          position = position_dodge(),
                          fill="darkcyan") +
        ggplot2::ylim(0,100)+
        ggplot2::labs(x = "Respuesta", y = "Porcentaje") +
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.6)+
        ggtitle(titulo)+
        theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                         hjust=0.5,face="bold",
                                         color="black", lineheight=1.5))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

      return(grafico)
    } else{
      grafico=data %>%
        ggplot2::ggplot(aes(x=reorder(vx,vy), y=vy)) +
        ggplot2::geom_bar(stat="identity",
                          position = position_dodge(),
                          fill="darkcyan") +
        ggplot2::ylim(0,100)+
        ggplot2::coord_flip()+
        ggplot2::labs(x = "Respuesta", y = "Porcentaje") +
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=0.5,hjust=0)+
        ggtitle(titulo)+
        theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                         hjust=0.5,face="bold",
                                         color="black", lineheight=1.5))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

      return(grafico)

    }
  } else{
    if(isFALSE(flip)){
      grafico=data %>%
        ggplot2::ggplot(aes(x=vx, y=vy, fill = grupo )) +
        ggplot2::geom_bar(stat="identity", position = position_dodge(),fill="darkcyan") +
        ggplot2::ylim(0,100)+
        ggplot2::labs(x = "Respuesta", y = "Porcentaje") +
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,
                           position = position_dodge(width = 1))+
        ggtitle(titulo)+
        theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                         hjust=0.5,face="bold",
                                         color="black", lineheight=1.5))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

      return(grafico)
    } else{
      grafico=data %>%
        ggplot2::ggplot(aes(x=vx, y=vy, fill = grupo )) +
        ggplot2::geom_bar(stat="identity", position = position_dodge(),fill="darkcyan") +
        ggplot2::ylim(0,100)+
        ggplot2::coord_flip()+
        ggplot2::labs(x = "Respuesta", y = "Porcentaje") +
        ggplot2::geom_text(aes(label=Porc,group=grupo),
                           size=3.5, vjust=0.5,hjust=-0.5,
                           position = position_dodge(width = 1),
                           inherit.aes = TRUE)+
        ggtitle(titulo)+
        theme (plot.title = element_text(size=rel(1),vjust=0.5,
                                         hjust=0.5,face="bold",
                                         color="black", lineheight=1.5))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

      return(grafico)
    }
  }
}
