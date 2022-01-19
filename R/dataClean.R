#' dataClean
#'
#' @title Consulta Saldo: dataClean
#'
#' @description Delete duplicates in user and cellphone columns
#'
#' @param data         {Data from survey}
#' @param email_column {Variable about participant email}
#'
#' @usage dataClean(
#' data = survey,
#' email_column = 4,
#' )
#'
#' @return
#' @export
#'
#' @examples
dataClean=function(data,email_column){
  data=as.data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))
  data=data %>%
    distinct(correo,id_encuesta,.keep_all=TRUE) %>%
    distinct(correo,cel,id_encuesta,.keep_all=TRUE)
  data=data[!duplicated(data$cel,incomparables = NA),]
  data[,c("correo","cel")] <- lapply(data[,c("correo","cel")], gsub, pattern = " ", replacement = "")
  data=as.data.frame(data)
  data$cel=nuevedigits(data$cel,9)


  #correo
  data=data %>%
    separate(correo,c("usuario","email"),sep="@")
  data$usuario=tolower(data$usuario)
  data[,c("usuario","email")] <- lapply(data[,c("usuario","email")], gsub, pattern = " ", replacement = "")
  data=as.data.frame(data)
  data=data %>% distinct(usuario,id_encuesta,.keep_all = TRUE)

  data[grep("gmm|g.mail|gamail|gamail|ghomil|gamil|gemail|gemil|gimail|gimeil|gm|gmai|gmail|Gmail|gmail|Gmail|gmall|GMAIL|gm|gmall|gmaill|gmai|gmal|gmaul|meil|gmil|gmila|gmill|gmqil|gomiel",data$email),email_column]="gmail.com"
  data[grep("autlook.com|outloock.com|outlook.com.pe|OUHOOK",data$email),email_column]="outlook.com"
  data[grep("7hotmail|godmail|gotmail|hitmail|hmail|hmil|4hotmaqil.com|hotail.com|homail|homil|hormail|hot|HOT|Hot|hoy|htmail",data$email),email_column]="hotmail.com"
  data[grep("outloo.es|Outlook.es",data$email),email_column]="outlook.es"
  data[grep("yhoo.es",data$email),email_column]="yahoo.es"
  data[grep("yahoo.com.pe|yahoo.com.ve",data$email),email_column]="yahoo.com"

  data=data  %>%
    mutate(correo=paste0(usuario,"@",email)) %>%
    mutate(correo=ifelse(correo=="NA@NA",NA,correo))
}
