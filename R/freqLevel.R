#' freqLevel
#'
#' @title Consulta Saldo: freqLevel
#'
#' @description Proportion table by levels
#'
#' @param data  {Survey data}
#' @param rango {Numbers of levels}
#'
#' @return
#' @export
#'
#' @examples
freqLevel = function(data,rango){
  test=data %>% select(rango)
  tab=as.data.frame(names(test)) %>%
    rename(names=1) %>%
    mutate(number=nchar(names)) %>%
    filter(number!=5)
  names=tab$names
  tab2=test %>% select(all_of(names))%>%
    slice(-1) %>%
    gather(alternativa,nivel) %>%
    select(-1)
  niveles=tab2 %>%
    group_by(nivel) %>%
    summarise(total=n())%>%
    ungroup() %>%
    dplyr::summarize(nivel,total,TOT = sum(total))%>%
    dplyr:: mutate(Porc = (total/TOT)*100,
                   Porc = substr(Porc,1,4),
                   Porc = as.numeric(Porc)) %>% select(-TOT)

  return(niveles)
}
