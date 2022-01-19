#' processData
#'
#' @title Consulta Saldo: processData
#'
#' @description Shows a table with absolute frequencies by users and surveys
#'
#' @param data    {Data survey}
#' @param ganador {User of survey winner}
#' @param fecha   {Date information about survey}
#'
#' @return
#' @export
#'
#' @examples
#'
processData=function(data,ganador,fecha){
  data1.1=data %>%
    select(registro,usuario,cel,id_encuesta) %>%
    mutate(registro=fecha(registro)) %>%
    arrange(registro)%>%
    mutate(id = as.numeric(row_number()))  %>%
    mutate(bono_cien=case_when(id %in% c(1:100)~1,
                               id %in% c(101:200)~0.5,
                               TRUE~0))%>%
    mutate(bono_respuesta=ifelse(bono_cien==0,0.25,0))%>%
    mutate(ganador=ifelse(usuario %in% c(ganador),1,0)) %>%
    mutate(bono_cien=case_when(ganador==1~0,
                               TRUE~bono_cien)) %>%
    select(-id)

  data1.1=data1.1[,c(4,1:3,5:7)]

  return(data1.1)
}
