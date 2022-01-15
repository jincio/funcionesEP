#' vote
#'
#' @title vote
#'
#' @description Calculate percentages by votes
#'
#' @param tipo   {The type of votes: General, Sin, Superior}
#' @param data   {The data to be displayed}
#' @param vx     {Variable in the x-axis}
#' @param vy     {Variable in the y-axis}
#' @param grupo  {The grouping variable}
#' @param dates  {Date information}
#' @param titulo {The title of the table}
#' @param reg    {Specifying region}
#'
#' @return
#' @export
#'
#' @examples
vote=function(tipo,data,vx,vy,grupo,dates,titulo,reg=FALSE){
  if(isFALSE(reg)){
    if(tipo=="General"){
      grafico=ggplot(data,
                     mapping = aes(x=vx,
                                   y=vy,
                                   group = grupo,
                                   color = grupo)) +
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        ggplot2::ylim(0,24)+
        scale_color_manual(values=c("cyan4","darkgreen","deeppink3",
                                    "gold3","lightcyan4","navyblue","salmon2",
                                    "coral4","turquoise2","green3","darkolivegreen4",
                                    "orchid","red"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        theme(legend.position = "bottom")

      return(grafico)
    }
    if(tipo=="Sin"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato,
                     mapping = aes(x=tiempo,
                                   y=Porc,
                                   group = respuesta,
                                   color = respuesta)) +
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        ggplot2::ylim(0,18)+
        scale_color_manual(values=c("cyan4","darkgreen","deeppink3","gold3",
                                    "lightcyan4","navyblue","salmon2","coral4",
                                    "turquoise2","green3","darkolivegreen4","orchid"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",
                                        color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        labs(color="Candidato")+
        theme(legend.position = "bottom")

      return(grafico)

    }
    if(tipo=="Superior"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato %>% filter(Porc>=5),
                     mapping = aes(x=tiempo,
                                   y=Porc,
                                   group = respuesta,
                                   color = respuesta)) +
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        ggplot2::ylim(0,18)+
        scale_color_manual(values=c("chocolate2","darkgreen","deeppink3","gold3",
                                    "lightcyan4","navyblue","salmon2","coral4",
                                    "turquoise2","green3","darkolivegreen4","orchid"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",
                                        color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        labs(color="Candidato")+
        theme(legend.position = "bottom")

      return(grafico)

    }

  } else{
    if(tipo=="General"){
      grafico=ggplot(data,
                     mapping = aes(x=vx,
                                   y=vy,
                                   group = grupo,
                                   color = grupo)) +
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        facet_wrap(~REGION,ncol=2)+
        ggplot2::ylim(0,13)+
        scale_color_manual(values=c("cyan4","darkgreen","deeppink3",
                                    "gold3","lightcyan4","navyblue","salmon2",
                                    "coral4","turquoise2","green3","darkolivegreen4",
                                    "orchid","red"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        theme(legend.position = "bottom")

      return(grafico)
    }
    if(tipo=="Sin"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato,
                     mapping = aes(x=tiempo,
                                   y=Porc,
                                   group = respuesta,
                                   color = respuesta))+
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        facet_wrap(~REGION,ncol=2)+
        ggplot2::ylim(0,14)+
        scale_color_manual(values=c("chocolate2","darkgreen","deeppink3",
                                    "gold3","lightcyan4","navyblue","salmon2",
                                    "coral4","turquoise2","green3","darkolivegreen4","orchid"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        labs(color="Respuesta")+
        theme(legend.position = "bottom")

      return(grafico)
    }
    if(tipo=="Superior"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato %>% filter(Porc>=5),
                     mapping = aes(x=tiempo,
                                   y=Porc,
                                   group = respuesta,
                                   color = respuesta)) +
        geom_line(size=0.9) +
        scale_x_date(breaks = as.Date(dates),
                     date_labels =  "%d %b %y") +
        geom_point( size=2, shape=21, fill="white")+
        labs(x="Encuesta", y="Respuesta")+
        facet_wrap(~REGION,ncol=2)+
        ggplot2::ylim(0,8)+
        scale_color_manual(values=c("chocolate2","darkgreen","deeppink3","gold3",
                                    "lightcyan4","navyblue","salmon2","coral4",
                                    "turquoise2","green3","darkolivegreen4","orchid"))+
        ggplot2::geom_text(aes(label=Porc), size=3.5, vjust=-0.5,inherit.aes = T,fontface="bold")+
        ggtitle(titulo)+
        theme(plot.title = element_text(size = rel(1),vjust=0.5,hjust=0.5,face="bold",color="black",lineheight = 1.5))+
        theme(axis.text.y = element_text(hjust = 1,size=10,face="bold"))+
        theme(axis.text.x = element_text(hjust=0.5,size = 8.5,face="bold"))+
        labs(color="Candidato")+
        theme(legend.position = "bottom")

      return(grafico)
    }
  }
}
