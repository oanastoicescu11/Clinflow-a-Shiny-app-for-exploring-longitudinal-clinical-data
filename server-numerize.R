output$dataset_numerize <- renderUI({
    #dataset choice UI element
    selectInput(
        "dataset_numerize",
        label = "Choose Data",
        choices = c("patient data", "visit data"),
        selected = "patient data",
        multiple = FALSE
    )
})

output$variable_choice_numerize <-
    renderUI({
        #variable choice ui element
        if (!is.null(original_data())) {
            req(input$dataset_numerize)
            if (input$dataset_numerize == "patient data") {
                selectInput(
                    "vars_to_numerize",
                    label = "Choose a categorical variable to convert to numeric",
                    choices = names(cbind(
                        select_if(datasets$constant_data, is.factor)
                    )),
                    multiple = FALSE
                )
            } else if (input$dataset_numerize == "visit data") {
                selectInput(
                    "vars_to_numerize",
                    label = "Choose a categorical variable to convert to numeric",
                    choices = names(cbind(
                        select_if(datasets$nonconstant_data, is.factor)
                    )),
                    multiple = FALSE
                )
                
            }
        }
    })

output$numerize_order <-
    renderUI({
        #text input for intervals UI element
        if (!is.null(original_data())) {
            req(input$dataset_numerize, input$vars_to_numerize)
            if (input$dataset_numerize == "patient data") {
                selectInput(
                    "numerize_order",
                    label = "Choose the order of values to be translated to numeric. The first value corresponds to numeric value 1",
                    choices = levels(datasets$constant_data[, input$vars_to_numerize]),
                    multiple = TRUE
                )
            } else if (input$dataset_numerize == "visit data") {
                selectInput(
                    "numerize_order",
                    label = "Choose the order of values to be translated to numeric. The first value corresponds to numeric value 1",
                    choices = levels(datasets$nonconstant_data[, input$vars_to_numerize]),
                    multiple = TRUE
                )
                
            }
        }
        
    })


numerize <-
    function(data,
             new_name,
             var_to_numerize,
             numerize_order) {
        #function to numerize variable
        num <-
            as.numeric(as.character(factor(
                data[, var_to_numerize],
                levels = as.character(numerize_order),
                labels = c(1:length(numerize_order))
            )))
        data$num <- num
        colnames(data)[colnames(data) == "num"] <- new_name
        
        return(data)
    }

observeEvent(input$update_numerize, {
    if (!is.null(original_data())) {
        req(input$dataset_numerize)
        if (input$dataset_numerize == "patient data") {
            data <- datasets$constant_data
        } else if (input$dataset_numerize == "visit data") {
            data <- datasets$visit_data
        }
        
        if (!is.null(input$numerize_order)) {
            if (input$new_num_name == "") {
                output$type_num_name <-
                    renderPrint(print("Please type a name for your new variable"))
            } else if (exists(input$new_num_name, where = data)) {
                output$choose_different_num_name <-
                    renderPrint(print(
                        "That variable name already exists. Choose a different name"
                    ))
            } else{
                data <-
                    numerize(
                        data,
                        input$new_num_name,
                        input$vars_to_numerize,
                        input$numerize_order
                    )
                output$type_num_name <- NULL
                output$choose_different_num_name <- NULL
            }
        }
        
        
        output$view_numerize <-
            DT::renderDT({
                #render table of the data with the new categorical variable
                datasets$newnum
            }, options = list(pageLength = 5))
        req(input$new_num_name)
        if (exists(input$new_num_name, where = data)) {
            if (all(is.na(data[, input$new_num_name]))) {
                output$norows_num <- renderPrint(print("NA added for all rows"))
            } else{
                output$norows_num <- NULL
            }
        }
        
        datasets$newnum <- data
        
        
    }
})

observeEvent(input$save_newvar_numerize_btn, {
    #save new data and overwrite to the reactive dataset
    if (!is.null(datasets$newnum)) {
        req(input$dataset_numerize)
        if (input$dataset_numerize == "patient data") {
            datasets$constant_data <- datasets$newnum
            if (!exists(input$new_num_name, where = datasets$visit_data)) {
                output$same_new_name <- NULL
                datasets$visit_data <-
                    left_join(datasets$visit_data, datasets$newnum[, c("code", input$new_num_name)], by = "code")
            } else{
                output$same_new_name <-
                    renderPrint(
                        print(
                            "There is a visit variable with the same name. Type a different name for your variable"
                        )
                    )
            }
        } else if (input$dataset_numerize == "visit data") {
            datasets$visit_data <- datasets$newnum
            
        } else{
            output$nothing_to_num_save <-
                renderPrint(print("Nothing to save. Keeping original table"))
        }
    }
    
})

#render table
output$check_numerize <-
    DT::renderDT({
        req(input$dataset_numerize)
        if (input$dataset_numerize == "patient data") {
            datasets$constant_data
        } else if (input$dataset_numerize == "visit data") {
            datasets$visit_data
            
        }
    }, options = list(pageLength = 5))

output$structure_numerize <- renderPrint({
    req(input$dataset_numerize)
    if (input$dataset_numerize == "patient data") {
        str(datasets$constant_data)
    } else if (input$dataset_numerize == "visit data") {
        str(datasets$visit_data)
        
    }
})

output$summary_numerize <- renderPrint({
    req(input$dataset_numerize)
    if (input$dataset_numerize == "patient data") {
        summary(datasets$constant_data)
    } else if (input$dataset_numerize == "visit data") {
        summary(datasets$visit_data)
        
    }
})