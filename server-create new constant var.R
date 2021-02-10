


output$range_var <-  renderUI({
  if (!is.null(original_data())) {
    data <- datasets$visit_data
    
    if (exists(input$choose_id, where = data)) {
      nonconstcols <-
        as.vector(find_nonconstant_cols(data, input$choose_id))
    }
    nonconstdata <- data[, nonconstcols]
    variable_choices <- NULL
    variable_choices <-
      names(cbind(
        select_if(nonconstdata, is.numeric),
        #select_if(nonconstdata, is.ordered),
        select_if(nonconstdata, lubridate::is.Date)
      ))
    selectInput(
      "var_range",
      label = "Choose a time variable",
      choices = variable_choices
      ,
      multiple = FALSE
    )
    
  }
  
})

output$newvar_params <- renderUI({
  #define UI for parameters
  if (!is.null(original_data())) {
    data <- datasets$visit_data
    
    if (exists(input$choose_id, where = data)) {
      nonconstcols <-
        as.vector(find_nonconstant_cols(data, input$choose_id))
    }
    nonconstdata <- data[, nonconstcols]
    variable_choices <- NULL
    variable_choices <-
      names(cbind(
        select_if(nonconstdata, is.numeric),
        select_if(nonconstdata, is.factor)
      ))
    req(input$var_range)
    tagList(
      
      sliderInput(
        "age_range",
        label = "Select time range of visits to aggregate",
        min = min(nonconstdata[, input$var_range], na.rm = T),
        max = max(nonconstdata[, input$var_range], na.rm = T),
        step = 1,
        value = c(min(nonconstdata[, input$var_range], na.rm = T), max(nonconstdata[, input$var_range], na.rm = T))
      ),
      
      # },
      selectInput(
        "var_to_apply_func",
        label = "Choose variable to aggregate",
        choices = variable_choices,
        multiple = FALSE
      )
      
    )
  }
  
})

output$func_to_apply <- renderUI({
  #define UI to choose function
  if (!is.null(original_data())) {
    data <- datasets$visit_data
    selectedvar <- data[, c(input$var_to_apply_func)]
    if (is.numeric(selectedvar)) {
      return(
        selectInput(
          "func_to_apply",
          label = "Choose aggregation method",
          choices = c("sum", "max", "min", "mean"),
          multiple = FALSE
        )
      )
    } else if (is.factor(selectedvar)) {
      return(
        selectInput(
          "func_to_apply",
          label = "Choose a level to mark TRUE for the period",
          choices = levels(selectedvar),
          multiple = TRUE
        )
      )
    }
  }
})


create_new_var <-
  function(constant,
           visit,
           age_range,
           var_to_apply_func,
           newvar_name,
           func_to_apply) {
    #function to create new variable by aggregating
    
    if (exists(input$var_range, where = visit)) {
      if (any(visit[, input$var_range] >= age_range[1] &
              visit[, input$var_range] <= age_range[2])) {
        visit <-
          visit[which(visit[, input$var_range] >= age_range[1] &
                        visit[, input$var_range] <= age_range[2]),]
      } else{
        visit <- NULL
      }
    } else{
      visit <- visit
    }
    if (!is.null(visit)) {
      if (is.numeric(visit[, var_to_apply_func])) {
        aggregated <-
          setNames(
            aggregate(
              visit[, var_to_apply_func] ~ visit[, input$choose_id],
              data = visit,
              FUN = func_to_apply,
              na.rm = T,
              na.action = na.pass
            ),
            c(input$choose_id, newvar_name)
          )
        if (any(aggregated[, newvar_name] == Inf)) {
          aggregated[aggregated[, newvar_name] == Inf, ][, newvar_name] <- NA
        }
        if (any(aggregated[, newvar_name] == -Inf)) {
          aggregated[aggregated[, newvar_name] == -Inf, ][, newvar_name] <- NA
        }
      } else if (is.factor(visit[, var_to_apply_func])) {
        aggregated <- visit[, c(input$choose_id, var_to_apply_func)] %>%
          split(visit[, input$choose_id]) %>%
          lapply(function(x) {
            mutate(x, newcol = ifelse(any(x[, var_to_apply_func] == func_to_apply), TRUE, FALSE))
            
          }) %>%
          do.call(rbind, .)
        
        colnames(aggregated)[colnames(aggregated) == "newcol"] <-
          newvar_name
        aggregated <-
          aggregated[!duplicated(aggregated[, c(input$choose_id, newvar_name)]), c(input$choose_id, newvar_name)]
        aggregated[, newvar_name] <-
          as.factor(aggregated[, newvar_name])
        
      }
      constant <-
        left_join(constant, aggregated, by = input$choose_id)
    } else{
      constant[, newvar_name] <- NA
    }
    return(constant)
    
  }


observeEvent(input$update, {
  # aggregate from visit data and add new variable
  
  
  if (!is.null(original_data())) {
    visit <- datasets$visit_data
    
    constants <- datasets$constant_data
    
    if (input$newvar_name == "") {
      output$type_name <-
        renderPrint(print("Please type a name for your new variable"))
    } else if (exists(as.character(input$newvar_name), where = constants)) {
      output$choose_different_name <-
        renderPrint(print("That variable name already exists. Choose a different name"))
    } else{
      suppressWarnings(
        newconstants <-
          create_new_var(
            constants,
            visit,
            age_range = input$age_range,
            var_to_apply_func = input$var_to_apply_func,
            newvar_name = input$newvar_name,
            func_to_apply = input$func_to_apply
          )
      )
      output$type_name <- NULL
      output$choose_different_name <- NULL
      
      
      if (exists("birth_date", where = newconstants)) {
        newconstants$birth_date <- as.Date(newconstants$birth_date)
      }
      
      
      output$view <-
        DT::renderDT({
          newconstants
        }, options = list(pageLength = 5))
      
      if (all(is.na(newconstants[, input$newvar_name]))) {
        output$norows <-
          renderPrint(print(
            "No visits in that age rage for any patient. NA added for all rows"
          ))
      } else{
        output$norows <- NULL
      }
      
      datasets$newconstants <- newconstants
    }
  }
  
})

output$save_newvar_btn <-
  renderUI({
    actionButton("save_newvar_btn", "Save Table")
  })
observeEvent(input$save_newvar_btn, {
  #save and rewrite to the reactive dataset, grouping by code
  if (!is.null(datasets$newconstants)) {
    datasets$constant_data <- datasets$newconstants
    datasets$visit_data <-
      left_join(datasets$visit_data,
                datasets$newconstants[, c(input$choose_id, input$newvar_name)],
                by = input$choose_id)
    output$nothing_to_save <- NULL
  } else{
    output$nothing_to_save <-
      renderPrint(print("Nothing to save. Keeping original table"))
  }
  
})

output$structure <-
  renderPrint({
    str(datasets$constant_data)
  })
output$new_summary <-
  renderPrint({
    summary(datasets$constant_data)
  })

#})

# create data output for selected variables
output$check <-
  DT::renderDT({
    datasets$constant_data
  }, options = list(pageLength = 5))

output$downloadNewConstantData <-
  downloadHandler(
    filename = function() {
      paste("constant_new", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasets$constant_data,
                file, row.names = FALSE)
    }
  )
