#' timeSection
#'
#' @title Consulta Saldo: timeSection
#'
#' @description Calculates completion time by users in every page break of qualtrics
#'
#' @param data           {Survey data}
#' @param var1           {Question's label}
#' @param var2           {Frecuency of responses by question}
#' @param submit_bloque1 {Column's position}
#'
#' @return
#' @export
#'
#' @examples
#'
timeSection=function(data,var1,var2,submit_bloque1){
  names=names(data)

  flags=grep("Page", names)
  ## esto funciona a partir del 2
  general=data.frame()

  for (i in 2:length(flags)){
    bloque=i
    index1=flags[i-1]+1
    index2=flags[i]
    test3=data[,index1:index2]
    ncol=ncol(test3)
    ind <- apply(test3, 1, function(x) all(is.na(x)))
    colnames(test3)[ncol]<-"submit"
    test4=test3%>%slice(-1)
    test4=test4%>%
      mutate(ID=1:n())%>%
      gather(pregunta,valor, -c(ID,"submit"))%>%## Aquí falta agregar
      ## algo para que se seleccione la
      select(-valor)%>%
      unique()%>%
      group_by(ID)%>%
      mutate(Np=n())%>%
      ungroup()%>%
      mutate(tiempo=as.numeric(submit))%>%
      select(pregunta,tiempo, ID, Np)%>%
      group_by(ID)%>%
      summarise(preguntas=paste(pregunta,collapse = ","),
                tiempo=min(tiempo),
                Np=min(Np))%>%
      ungroup()%>%
      mutate(bloque=bloque) %>%
      group_by(bloque) %>%
      mutate(x=complete.cases(tiempo)) %>%
      filter(x=="TRUE") %>%
      select(-x)


    general=dplyr::bind_rows(general, test4)
  }

  bloque1=data%>%
    select(all_of(var1),all_of(var2),all_of(submit_bloque1))%>%
    slice(-1)%>%
    na.omit()%>%
    mutate(ID=1:n())%>%
    gather(pregunta,valor, -c(ID,submit_bloque1))%>%
    select(-valor)%>%
    unique()%>%
    group_by(ID)%>%
    mutate(Np=n())%>%rename(tiempo=1) %>%
    ungroup()%>%
    mutate(tiempo=as.numeric(tiempo))%>%
    select(pregunta,tiempo, ID, Np)%>%
    group_by(ID)%>%
    summarise(preguntas=paste(pregunta,collapse = ","),
              tiempo=min(tiempo),
              Np=min(Np))%>%
    ungroup()%>%
    mutate(bloque=1)

  general=rbind(general,bloque1)
  general=general %>%
    arrange(bloque)

  return(general)
}
