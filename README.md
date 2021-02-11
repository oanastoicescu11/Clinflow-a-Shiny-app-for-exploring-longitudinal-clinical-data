# Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data
## Online demo
An online demo of this app can be found [here](https://oanastoicescu11.shinyapps.io/Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data/)
## To run locally:
- Install the R software on your computer. R version 3.6.3 can be found [here]( https://cran.r-project.org/bin/windows/base/old/3.6.3/)
- Download the source code files in a folder on your computer. 
- open R and set that folder as your working directory using the command `setwd("C:/<your folder path here>")`
- Install the packages using the Libraries.R file with the command `source("Libraries.R")`
- Run the app with the command:
  `shiny::runApp(launch.browser = TRUE)` to run the app in your default browser that supports WebGL (recommended: Google Chrome, Mozilla Firefox)
