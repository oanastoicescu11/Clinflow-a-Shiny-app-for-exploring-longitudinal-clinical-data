#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Function To check if libraries are installed and load/install them

library("dashboardthemes")
library("shinydashboard")
library("shiny")
library("shinythemes")
library("shinyjs")
library("shinyWidgets")
library("plyr")
library("ggplot2")
library("dplyr")

library("gridExtra")
library("corrplot")
library("openxlsx")
library("janitor")
library("zoo")
library("purrr")
library("purrrlyr")
library("tidyverse")
library("eeptools")
library("Amelia")


library("htmltools")
library("jsonlite")
library("stringi")
library("rlang")
library("htmlwidgets")
library("DT")
library("naniar")
library("ggplot2")
library("shinyBS")

library("readr")
library("svglite")
library("pheatmap")
library("ade4")
library("kohonen")
library("factoextra")
library("RColorBrewer")
library("ggcorrplot")
library("caret")
library("ggpubr")
library("Rtsne")
library("plot3D")
library("ape")
library("scatterplot3d")
library("car")
library("lubridate")
library("esquisse")
library("plotly")
library("GGally")
library("ggiraph")
library("ggforce")
library("survival")
library("survminer")
library("forestmodel")
library("devtools")
library("AppliedPredictiveModeling")
library("ellipse")
library("BiocManager")
library("pcaMethods")



shinyUI(tagList(
  #load UI files
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyDashboardThemes(theme = "poor_mans_flatly"),
      navbarPage("ClinFlow",
        theme = shinytheme("flatly"),
        # <--- To use a theme, uncomment this
        source("ui-tab-inputdata.R", local = TRUE)$value,
        source("UI-analysis.R", local = TRUE)$value,
        source("ui-panel.R", local = TRUE)$value,
        source("UI-survival.R", local = TRUE)$value,
        source("ui-documentation.R", local = TRUE)$value
      )
    )
  )
))
