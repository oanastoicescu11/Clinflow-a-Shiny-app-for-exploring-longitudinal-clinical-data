checkInstallLoad <- function(libName)
{
    if (!require(libName, character.only = TRUE))
    {
        install.packages(libName, dependencies = TRUE)
    }
}

#install libraries-
checkInstallLoad("shiny")
checkInstallLoad("shinythemes")
checkInstallLoad("shinyjs")
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
checkInstallLoad("shinydashboard")
checkInstallLoad("DT")
checkInstallLoad("naniar")
checkInstallLoad("ggplot2")
checkInstallLoad("shinyBS")

checkInstallLoad("readr")
checkInstallLoad("svglite")
checkInstallLoad("pheatmap")
checkInstallLoad("ade4")
checkInstallLoad("kohonen")
checkInstallLoad("factoextra")
checkInstallLoad("RColorBrewer")
checkInstallLoad("ggcorrplot")
checkInstallLoad("caret")
checkInstallLoad("ggpubr")
checkInstallLoad("Rtsne")
checkInstallLoad("plot3D")
checkInstallLoad("ape")
checkInstallLoad("scatterplot3d")
checkInstallLoad("car")
checkInstallLoad("lubridate")
checkInstallLoad("esquisse")
checkInstallLoad("plotly")
checkInstallLoad("GGally")
checkInstallLoad("ggiraph")
checkInstallLoad("ggforce")
checkInstallLoad("survival")
checkInstallLoad("survminer")
checkInstallLoad("forestmodel")

checkInstallLoad("AppliedPredictiveModeling")
if (!requireNamespace("ellipse", quietly = TRUE))
    install.packages("ellipse")

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

if (!require("pcaMethods", character.only = TRUE))
{
    BiocManager::install("pcaMethods")
}


if (!require(devtools))
    install.packages("devtools")
if (!require("dashboardthemes")) {
    devtools::install_github("nik01010/dashboardthemes", build_vignettes = FALSE)
} 


