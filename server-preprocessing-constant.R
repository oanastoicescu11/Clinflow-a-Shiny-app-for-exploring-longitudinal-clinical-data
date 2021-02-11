

#define function for extracting and preprocessing visit data
#+ creating more relevant variables

source("fun_logical_to_factors.R")
source("fun_remove_duplicated.R")

#define function for extracting the constant data
create_constant_table <- function(data) {
  #extract constant columns and put them only once
  #define the constant data columns
  #function to find constant cols
  if (exists(input$choose_id, where = data)) {
    constcols <- as.vector(find_constant_cols(data, input$choose_id))
  }
  
  
  new <- as.data.frame(data[, constcols])
  newdata <- new[!duplicated(new),]
  
  
  return(newdata)
}


#define the visit data
visit_data <- reactive({
  req(original_data(), input$choose_id)
  withProgress(value = 0.5,
               message = "Preprocessing visit Data ",
               detail = "...",
               {
                 visit <-
                   logical_to_factors(remove_duplicated(original_data())) #only converts logical to factor and removes duplicated entries
                 if (!is.null(visit)) {
                   visit <-
                     visit %>% mutate_if(is.factor, stringi::stri_trans_general, "latin-ascii")
                   visit <-
                     visit %>% mutate_if(is.character,
                                         stringi::stri_trans_general,
                                         "latin-ascii")
                   visit <-
                     visit %>% mutate_if(is.character, as.factor)
                   visit <-
                     visit[, colSums(is.na(visit)) < nrow(visit)]
                   
                 }
                 
                 setProgress(value = 1, detail = "done!")
               })
  visit
  
})

#extract contant columns from the visit data
constant_data <- reactive({
  req(original_data(), input$choose_id)
  withProgress(value = 0.5,
               message = "Preprocessing constant Data ",
               detail = "...",
               {
                 if (!is.null(visit_data())) {
                   req(exists(input$choose_id, where = visit_data()))
                   const <- data.frame(create_constant_table(visit_data()))
                   setProgress(value = 1, detail = "done!")
                   rownames(const) <- c(1:nrow(const))
                   if (ncol(const) == 1) {
                     colnames(const) <- input$choose_id
                     const$rownumber <- rep(1:nrow(const))
                   }
                 } else{
                   const <- NULL
                 }
               })
  const
  
})

#render tables and summaries
output$table_constants <- DT::renderDT({
  constant_data()
}, options = list(pageLength = 5))

#observeEvent(input$view_constant_summary,{
output$summary_constant <-  renderPrint({
  summary(constant_data())
})
#})

#observeEvent(input$view_constant_structure,{
output$structure_constant <-  renderPrint({
  str(constant_data())
})
#})

output$downloadConstantData <-
  downloadHandler(
    filename = function() {
      paste("constant", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(constant_data(),
                file, row.names = FALSE)
    }
  )


output$table_visits <- DT::renderDT({
  visit_data()
}, options = list(pageLength = 5))

#observeEvent(input$view_visit_summary,{
output$summary_visit <-  renderPrint({
  summary(visit_data())
})
#})

#observeEvent(input$view_visit_structure,{
output$structure_visit <-  renderPrint({
  str(visit_data())
})
#})


output$downloadVisitData <- downloadHandler(
  filename = function() {
    paste("visit", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(visit_data(),
              file, row.names = FALSE)
  }
)

datasets <-
  reactiveValues() #reactive values to store the filtered and new data generated in the app


###############################
