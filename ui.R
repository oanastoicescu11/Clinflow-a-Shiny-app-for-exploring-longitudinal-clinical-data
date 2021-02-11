#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Function To check if libraries are installed and load/install them

checkInstallLoad <- function(libName)
{
  if (!require(libName, character.only = TRUE))
  {
    install.packages(libName, dependencies = TRUE)
    require(libName, character.only = TRUE)
  }
}

#load or install libraries-
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
library("ellipse")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!require("pcaMethods", character.only = TRUE))
{
  BiocManager::install("pcaMethods")
  require("pcaMethods", character.only = TRUE)
}


if (!require(devtools))
  install.packages("devtools")
if (!require("dashboardthemes")) {
  devtools::install_github("nik01010/dashboardthemes", build_vignettes = FALSE)
  library("dashboardthemes")
} else{
  library("dashboardthemes")
}




shinyUI(tagList(
  #load UI files
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyDashboardThemes(theme = "poor_mans_flatly"),
      navbarPage(
        strong(icon("chart-area")),
        theme = shinytheme("flatly"),
        # <--- To use a theme, uncomment this
        source("ui-tab-inputdata.R", local = TRUE)$value,
        source("UI-analysis.R", local = TRUE)$value,
        source("ui-panel.R", local = TRUE)$value,
        source("UI-survival.R", local = TRUE)$value
        
      )
    )
  )
))
