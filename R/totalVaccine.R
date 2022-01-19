#' totalVaccine
#'
#' @title totalVaccine
#'
#' @description A function to merge the vaccine surveys and create a summary by survey's date
#' @return
#' @export
#'
#' @examples
#'
totalVaccine=function(){
  #carga de data
  link_3enc="https://docs.google.com/spreadsheets/d/e/2PACX-1vTmhUtfjr3kf0OlhPKlmbvtTnPtmYt-Q-nEPNPiM97M7jq_4M819kSq6DbGwI9kBnc99RjMR1ZczSzA/pub?output=xlsx"
  link_4enc="https://docs.google.com/spreadsheets/d/e/2PACX-1vTZYdMm-Dc7Ie5phylmNntQETQdRFxL9nULUMAKHMHo9Qu9b0dWjfAw6Kxe6V-eANvasoTKmm1pMhTy/pub?output=xlsx"
  link_5enc="https://docs.google.com/spreadsheets/d/e/2PACX-1vTTu27bI0yaeBKP6oI32on6gsAeFYQqJajSvzBmoWtK6jDAhJOMkQLu_3Ezgd7wesM9J1qtVb5etQF5/pub?output=xlsx"
  link_6enc="https://docs.google.com/spreadsheets/d/e/2PACX-1vRgPzNpfoZ03i0oAz_0lLsDO9-8OGHsTJEvWXfh62UretER0srXa-xmEqFg-6oZMVjOmCHCGm2EODHx/pub?output=xlsx"

  enc3=read.xlsx(link_3enc)
  enc4=read.xlsx(link_4enc)
  enc4=enc4[!duplicated(enc4$Email.Address),]
  enc5=read.xlsx(link_5enc)
  enc5=enc5[!duplicated(enc5$Dirección.de.correo.electrónico),]
  enc6=read.xlsx(link_6enc)
  enc6=enc6[!duplicated(enc6$Dirección.de.correo.electrónico),]


  #subset con preguntas a analizar: vacunas
  enc3=enc3[,c(1,5,28,29)]
  enc4=enc4[,c(1,5,13,14)]
  enc5=enc5[,c(1,5,13,14)]
  enc6=enc6[,c(1,5,6,7,9)]

  names(enc3)=c("tiempo","departamento","oblig","dispo")
  names(enc4)=c("tiempo","departamento","oblig","dispo")
  names(enc5)=c("tiempo","departamento","oblig","pref")
  names(enc6)=c("tiempo","departamento","oblig","dispo","pref")

  #función para crear la fecha de las encuestas
  tiempo=function(data,fecha){
    tiempo=data %>%
      mutate(tiempo=as.Date(fecha,format="%Y-%m-%d"))
    return(tiempo)
  }

  #agregar fecha de encuestas
  enc3=tiempo(enc3,"2021-01-06")
  enc4=tiempo(enc4,"2021-02-11")
  enc5=tiempo(enc5,"2021-03-02")
  enc6=tiempo(enc6,"2021-03-18")

  #agregar total de muestra
  enc3=enc3 %>%
    mutate(TOTAL=542)
  enc4=enc4 %>%
    mutate(TOTAL=759)
  enc5=enc5 %>%
    mutate(TOTAL=480)
  enc6=enc6 %>%
    mutate(TOTAL=695)

  #unir todo
  vacu_enc1=rbind(enc3,enc4)
  vacu_enc=Reduce(function(x, y) merge(x, y, all=TRUE), list(vacu_enc1,enc5,enc6))

  #formato
  library(stringr)
  vacu_enc$TOTAL=as.numeric(vacu_enc$TOTAL)


  #estandarización de respuestas
  vacu_enc[grep("No lo sé",vacu_enc$oblig),3]="No sabe"
  vacu_enc[grep("No lo sé",vacu_enc$dispo),5]="No sabe"
  vacu_enc[grep("No me vacunaré",vacu_enc$dispo),5]="No se vacunaría"
  vacu_enc[grep("Sí me vacunaré",vacu_enc$dispo),5]="Sí se vacunaría"

  vacu_enc=vacu_enc %>%
    mutate(REGION=case_when(departamento %in% c("Cajamarca","La Libertad","Lambayeque","Piura","Tumbes") ~ "Norte",
                            departamento %in% c("Arequipa","Cusco","Moquegua","Madre de Dios","Tacna","Apurímac","Puno","Otro") ~ "Sur",
                            departamento %in% c("Áncash","Ancash","Ayacucho","Huancavelica","Huancaveliza","Huánuco","Junín","Pasco","Ica") ~ "Centro",
                            departamento %in% c("Lima","Callao")~ "Lima",
                            departamento %in% c("Amazonas","Loreto","San Martín","Ucayali")~"Oriente"))

  return(vacu_enc)
}
