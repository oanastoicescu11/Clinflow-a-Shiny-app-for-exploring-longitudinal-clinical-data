output$dataset_categorize <- renderUI({
  #dataset choice UI element
  selectInput(
    "dataset_categorize",
    label = "choose data",
    choices = c("patient data", "visit data"),
    selected = "patient data",
    multiple = FALSE
  )
})

output$variable_choice_categorize <- renderUI({
  #variable choice ui element
  if (!is.null(original_data())) {
    req(input$dataset_categorize)
    if (input$dataset_categorize == "patient data") {
      selectInput(
        "vars_to_categorize",
        label = "choose vars",
        choices = names(cbind(
          select_if(datasets$constant_data, is.numeric)
        )),
        multiple = FALSE
      )
    } else if (input$dataset_categorize == "visit data") {
      selectInput(
        "vars_to_categorize",
        label = "choose vars",
        choices = names(cbind(
          select_if(datasets$nonconstant_data, is.numeric)
        )),
        multiple = FALSE
      )
      
    }
  }
})

output$categorize_intervals <-
  renderUI({
    #text input for intervals UI element
    if (!is.null(original_data())) {
      textInput(
        "categorize_intervals",
        "Choose cut-off points (integers or numbers with decimals after point)
                to create the category by interval (such as: 0,1,2,...): ",
      )
      
    }
    
  })


categorize <-
  function(data,
           newcat_name,
           var_to_categorize,
           categorize_intervals) {
    #function to categorize variable
    cat <-
      cut(
        data[, var_to_categorize],
        breaks = as.numeric(unlist(strsplit(
          categorize_intervals, ","
        )))
        ,
        include.lowest = TRUE,
        right = FALSE,
        ordered_result = TRUE,
        dig.lab = 10
      )
    
    data$cat <- cat
    data$cat <- droplevels(data$cat)
    colnames(data)[colnames(data) == "cat"] <- newcat_name
    
    return(data)
  }

observeEvent(input$update_categorize, {
  if (!is.null(original_data())) {
    req(input$dataset_categorize)
    if (input$dataset_categorize == "patient data") {
      data <- datasets$constant_data
    } else if (input$dataset_categorize == "visit data") {
      data <- datasets$visit_data
    }
    
    
    if (!is.null(input$categorize_intervals)) {
      if (input$newcat_name == "") {
        output$type_cat_name <-
          renderPrint(print("Please type a name for your new variable"))
      } else if (exists(input$newcat_name, where = data)) {
        output$choose_different_cat_name <-
          renderPrint(print(
            "That variable name already exists. Choose a different name"
          ))
      } else if (any(is.na(as.numeric(unlist(
        strsplit(input$categorize_intervals, ",")
      ))))) {
        output$invalid_vect <-
          renderPrint(print("Please type only comma delimited numbers: 1,2,3"))
      } else{
        data <-
          categorize(
            data,
            input$newcat_name,
            input$vars_to_categorize,
            input$categorize_intervals
          )
        output$type_cat_name <- NULL
        output$choose_different_cat_name <- NULL
        output$invalid_vect <- NULL
      }
    }
    
    
    output$view_categorize <-
      DT::renderDT({
        #render table of the data with the new categorical variable
        datasets$newcat
      }, options = list(pageLength = 5))
    req(input$newcat_name)
    if (exists(input$newcat_name, where = data)) {
      if (all(is.na(data[, input$newcat_name]))) {
        output$norows_cat <- renderPrint(print("NA added for all rows"))
      } else{
        output$norows_cat <- NULL
      }
    }
    
    datasets$newcat <- data
    
    
  }
})

observeEvent(input$save_newvar_categorize_btn, {
  #save new data and overwrite to the reactive dataset
  if (!is.null(datasets$newcat)) {
    req(input$dataset_categorize)
    if (input$dataset_categorize == "patient data") {
      datasets$constant_data <- datasets$newcat
      if (!exists(input$newcat_name, where = datasets$visit_data)) {
        output$same_name <- NULL
        datasets$visit_data <-
          left_join(datasets$visit_data, datasets$newcat[, c(input$choose_id, input$newcat_name)], by = input$choose_id)
      } else{
        output$same_name <-
          renderPrint(
            print(
              "There is a visit variable with the same name. Type a different name for your variable"
            )
          )
      }
    } else if (input$dataset_categorize == "visit data") {
      datasets$visit_data <- datasets$newcat
      
    } else{
      output$nothing_to_cat_save <-
        renderPrint(print("Nothing to save. Keeping original table"))
    }
  }
  
})

#render table
output$check_categorize <-
  DT::renderDT({
    req(input$dataset_categorize)
    if (input$dataset_categorize == "patient data") {
      datasets$constant_data
    } else if (input$dataset_categorize == "visit data") {
      datasets$visit_data
      
    }
  }, options = list(pageLength = 5))

output$structure_categorize <- renderPrint({
  req(input$dataset_categorize)
  if (input$dataset_categorize == "patient data") {
    str(datasets$constant_data)
  } else if (input$dataset_categorize == "visit data") {
    str(datasets$visit_data)
    
  }
})

output$summary_categorize <- renderPrint({
  req(input$dataset_categorize)
  if (input$dataset_categorize == "patient data") {
    summary(datasets$constant_data)
  } else if (input$dataset_categorize == "visit data") {
    summary(datasets$visit_data)
    
  }
})