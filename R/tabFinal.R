#' tabFinal
#'
#' @title Consulta Saldo: tabFinal
#'
#' @description Groups processed data by users, and joins it with withdrawals' table
#'
#' @param data    {processed data}
#' @param retiros {withdrawals colum}
#'
#' @return
#' @export
#'
#' @examples

tabFinal=function(data,retiros){
  dataCel=data %>%
    filter(!is.na(cel)) %>%
    group_by(cel) %>%
    filter(n()>1) %>%
    ungroup() %>%
    group_by(usuario,cel) %>%
    filter(n()==1)%>%
    group_by(cel) %>%
    summarise(encuestas_realizadas=n(),
              dinero_primeros_cien=sum(bono_cien[bono_cien==1]),
              dinero_segundos_cien=sum(bono_cien[bono_cien==0.5]),
              sorteos_ganados=sum(ganador)) %>%
    ungroup() %>%
    mutate(dinero_acumulado=select(., dinero_primeros_cien,dinero_segundos_cien) %>%
             rowSums(na.rm = TRUE))  %>%
    left_join(dataRetiro %>% select(cel,retirado),by='cel') %>%
    mutate(retirado=ifelse(is.na(retirado),0,retirado),
           diponible=dinero_acumulado-retirado)


  dataFinal=data%>%
    group_by(usuario) %>%
    summarise(encuestas_realizadas=n(),
              dinero_primeros_cien=sum(bono_cien[bono_cien==1]),
              dinero_segundos_cien=sum(bono_cien[bono_cien==0.5]),
              sorteos_ganados=sum(ganador)) %>%
    ungroup() %>%
    mutate(dinero_acumulado=select(., dinero_primeros_cien,dinero_segundos_cien) %>%
             rowSums(na.rm = TRUE)) %>%
    left_join(retiros %>% select(usuario,retirado),by='usuario') %>%
    mutate(retirado=ifelse(is.na(retirado),0,retirado),
           diponible=dinero_acumulado-retirado)

  return(dataFinal)
}
