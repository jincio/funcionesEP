library(devtools)
library(roxygen2)
library(usethis)

#devtools::install_github("jincio/funcionesEP")
library(funcionesEP) # cargar paquete

create("funcionesEP")

usethis::use_r("plotBarVoting")
devtools::document()
# devtools::check()
# usethis::use_mit_license()
usethis::use_package("tidyverse", "Suggests")

#devtools::install_github("jincio/funcionesEP")
usethis::use_git()

#usethis::browse_github_pat()
#create_github_token()
#usethis::use_github(protocol="https",
#                    auth_token = "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")
#devtools::ins


?funcionesEP::plotBar()

library(tidyverse)
library(datos)
pinguinos

data <- pinguinos %>%
  count(isla) %>%
  mutate(Porc = n/sum(n)*100) %>%
  mutate(Porc = round(Porc,2))

plotBar(data = data ,
        vx = data$isla,
        vy = data$Porc,
        titulo = "prueba",
        flip = T)





