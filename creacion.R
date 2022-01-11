library(devtools)
library(roxygen2)
library(usethis)
library(funcionesEP) # cargar paquete

create("funcionesEP")

usethis::use_r("plotBar")
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



