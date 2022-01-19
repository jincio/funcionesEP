#' statsNotFin
#'
#' @title Consulta Saldo: statsNotFin
#'
#' @description Customizes conjoint data. Function shows the specific number of columns
#'
#' @param name_data     {Data survey}
#' @param excluir_names {Exclude conjoint sections}
#' @param excluir_range {Joins variables with NA's }
#' @param n_columns     {Number of columns}
#'
#' @return
#' @export
#'
#' @examples
statsNotFin=function(name_data,excluir_names,excluir_range,n_columns){

  #import data
  df=rio::import(name_data)


  #cleaning columns
  test=df  %>%
    select(-contains(c("Click","Submit","Timing","atención"))) %>%
    select(-contains(excluir_names)) %>%
    select(-all_of(excluir_range))# %>%
  #filter(!is.na(Edad))

  print(length(test))

  #diccionario de preguntas
  cod_inc=data.frame(Var1=colnames(test),Posicion=1:n_columns)

  #seleccionar la ultima pregunta que ha llenado cada usuario
  pdTest=as.data.frame(apply(test, 1, function(x) { tail(names(x)[!is.na(x)], n=1) }))
  names(pdTest)=c("Last")

  #tabla de frecuencia
  frec_last=as.data.frame(table(pdTest$Last)) %>% arrange(desc(Freq))

  #unir resultado con la posición de la pregunta

  frec_last=frec_last %>% left_join(cod_inc,by="Var1") %>%
    rename(Pregunta=1,
           Total_usuarios=2,
           Numero_pregunta=3) %>% arrange(Numero_pregunta)

  return(frec_last)

}
