# Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data

This Shiny app is dedicated to analyzing clinical data, more specifically, longitudinal medical visit data. 
The app includes filtering, feature construction and outlier treatment, and a platform for visualizing different analyses.
The end goal is to facilitate the user to explore data, collect subsets of data based on their research hypothesis, extract new information and perform analyses. 

Disclaimer: This application has been developed with a specific dataset format in mind. It might not fit all types of data. The author does not assume any responsibility if the results are not what you expect.

## Online demo
An online demo of this app can be found [here](https://oanastoicescu11.shinyapps.io/Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data/)

## To run locally:
- Install the R software on your computer. R version 3.6.3 can be found [here]( https://cran.r-project.org/bin/windows/base/old/3.6.3/)
- Download the source code files in a folder on your computer. 
- open R and set that folder as your working directory using the command `setwd("C:/<your folder path here>")`
- Install the packages using the Libraries.R file with the command `source("Libraries.R")`
- Run the app with the command:
  `shiny::runApp(launch.browser = TRUE)` to run the app in your default browser that supports WebGL (recommended: Google Chrome, Mozilla Firefox)
