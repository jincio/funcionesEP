#' barVote
#'
#' @title barVote
#'
#' @description A barplot to visualize voting intention
#'
#' @param tipo   {The type of votes: General, Sin, Superior}
#' @param data   {The data to be displayed}
#' @param vx     {Variable in the x-axis}
#' @param vy     {Variable in the y-axis}
#' @param grupo  {The grouping variable}
#' @param titulo {The title of the table}
#' @param reg    {Specifying region}
#'
#' @return
#' @export
#'
#' @examples
barVote=function(tipo,data,vx,vy,grupo,titulo,reg=FALSE){
  if(isFALSE(reg)){
    if(tipo=="General"){
      grafico=ggplot(data,
                     aes(x=reorder(vx,-vy), y=vy, fill=as.factor(grupo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        ggtitle(titulo)

      return(grafico)
    }
    if(tipo=="Sin"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato,
                     aes(x=reorder(respuesta,-Porc), y=Porc, fill=as.factor(tiempo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        ggtitle(titulo)


      return(grafico)

    }
    if(tipo=="Superior"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato %>% filter(Porc>=5),
                     aes(x=reorder(respuesta,-Porc), y=Porc, fill=as.factor(tiempo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        ggtitle(titulo)

      return(grafico)

    }

  } else{
    if(tipo=="General"){
      grafico=ggplot(data,
                     aes(x=reorder(vx,-vy), y=vy, fill=as.factor(grupo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        facet_wrap(~REGION,ncol=1)+
        ggtitle(titulo)

      return(grafico)
    }
    if(tipo=="Sin"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato,
                     aes(x=reorder(respuesta,-Porc), y=Porc, fill=as.factor(tiempo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        facet_wrap(~REGION,ncol=1)+
        ggtitle(titulo)

      return(grafico)
    }
    if(tipo=="Superior"){
      aproba_candidato=data[-grep("Otro|sabe|Blanco/viciado",data$respuesta),]

      grafico=ggplot(aproba_candidato %>% filter(Porc>=5),
                     aes(x=reorder(respuesta,-Porc), y=Porc, fill=as.factor(tiempo))) +
        geom_bar(stat="identity", position = position_dodge()) +
        labs(x="Candidatos", y="% Intencion de voto",fill="Fecha")+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_fill_manual(values=c("#000066","red4","#339999","salmon2"))+
        ggplot2::geom_text(aes(label = Porc, group = tiempo),
                           position = position_dodge(width = 1),
                           vjust = -0.5, size = 2,fontface="bold")+
        theme(legend.position = "bottom")+
        scale_x_discrete(expand = c(0,0))+
        facet_wrap(~REGION,ncol=1)+
        ggtitle(titulo)

      return(grafico)
    }
  }
}
