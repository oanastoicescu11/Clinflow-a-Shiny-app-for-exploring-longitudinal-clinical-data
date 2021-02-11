#Function To check if
checkInstallLoad <- function(libName)
{
    if(!require(libName, character.only=TRUE))
    {
        install.packages(libName)
        require(libName, character.only=TRUE)
    }
}


checkInstallLoad("shiny")
checkInstallLoad("shinyWidgets")
checkInstallLoad("plyr")
checkInstallLoad("ggplot2")
checkInstallLoad("dplyr")
checkInstallLoad("gridExtra")
checkInstallLoad("corrplot")
checkInstallLoad("openxlsx")
checkInstallLoad("janitor")
checkInstallLoad("zoo")
checkInstallLoad("purrr")
checkInstallLoad("purrrlyr")
checkInstallLoad("tidyverse")
checkInstallLoad("eeptools")
checkInstallLoad("Amelia")


checkInstallLoad("htmltools")
checkInstallLoad("jsonlite")
checkInstallLoad("stringi")
checkInstallLoad("rlang")
checkInstallLoad("htmlwidgets")
checkInstallLoad("naniar")

# library('devtools')
# install_github('graemeleehickey/joineR', build_vignettes = FALSE)
# library('devtools')
# install_github('graemeleehickey/joineRML')
if(!require(devtools)) install.packages("devtools")
if(!require("esquisse")){
    devtools::install_github("dreamRs/esquisse", build_vignettes = FALSE)
    library("esquisse")
}else{ library("esquisse")}

# library(htmlwidgets)
# library(D3TableFilter)
# checkInstallLoad("htmlwidgets")
# checkInstallLoad("D3TableFilter")
# remotes::install_github("dreamRs/esquisse")