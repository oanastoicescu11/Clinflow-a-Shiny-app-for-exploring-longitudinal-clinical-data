



#choose data
output$dataset_choice <- renderUI({
    selectInput(
        "dataset_choice",
        label = "choose data",
        choices = c("patient data", "visit data"),
        selected = "patient data",
        multiple = FALSE
    )
})

observe({
    #put the visit data in a reactive value
    datasets$visit_data <- as.data.frame(visit_data())
})
observe({
    #observer that activates the filter only when data is loaded and pre-processed
    
    #put the patient data in a reactive value
    datasets$constant_data <- as.data.frame(constant_data())
    
    find_cols <- function(data) {
        const_cols <-
            function(x)
                ! sum(duplicated(data[c(x)])) == nrow(data) - 1
        return(Filter(const_cols, names(data)))
    }
    
    observe({
        #observer that updates the filter UI with the selected variables
        
        if (!is.null(datasets$visit_data)) {
            #find time variable columns
            data <- as.data.frame(datasets$visit_data)
            req(input$choose_id)
            if (exists(input$choose_id, where = data)) {
                nonconstcols <-
                    as.vector(find_nonconstant_cols(data, input$choose_id))
            }
            nonconstdata <- as.data.frame(data[, nonconstcols])
            #put only the time variable columns in a reactive value
            datasets$nonconstant_data <- as.data.frame(nonconstdata)
        }
    })
    
    #UI element to select variables to filter
    output$variable_choice <- renderUI({
        if (!is.null(original_data())) {
            req(input$dataset_choice)
            if (input$dataset_choice == "patient data") {
                selectInput(
                    "vars_to_filter",
                    label = "choose variables to filter",
                    choices = find_cols(datasets$constant_data),
                    multiple = TRUE,
                    selected = head(find_cols(datasets$constant_data))
                )
            } else if (input$dataset_choice == "visit data") {
                selectInput(
                    "vars_to_filter",
                    label = "choose variables to filter",
                    choices = colnames(datasets$visit_data),
                    multiple = TRUE,
                    selected = head(colnames(datasets$visit_data))
                )
                
            }
        }
    })
    
    output$variable2_choice <- renderUI({
        if (!is.null(original_data())) {
            req(input$dataset_choice)
            if (input$dataset_choice == "patient data") {
                selectInput(
                    "vars_to_group",
                    label = "choose variables to group",
                    choices = find_cols(datasets$constant_data),
                    multiple = TRUE,
                    selected = head(find_cols(datasets$constant_data))
                )
            } else if (input$dataset_choice == "visit data") {
                selectInput(
                    "vars_to_group",
                    label = "choose variables to group",
                    choices = colnames(datasets$visit_data),
                    multiple = TRUE,
                    selected = head(colnames(datasets$visit_data))
                )
                
            }
        }
    })
    
    # Create object for reactive values
    rv <- reactiveValues(value_store = character(),
                         group_store = character())
    
    
    # When input changes -> update
    observe({
        rv$value_store <- input$vars_to_filter
        rv$group_store <- input$vars_to_group
        if (!is.null(original_data())) {
            req(input$dataset_choice)
            if (input$dataset_choice == "patient data") {
                data <- as.data.frame(datasets$constant_data)
            } else if (input$dataset_choice == "visit data") {
                data <- as.data.frame(datasets$visit_data)
            }
        } else{
            data <- NULL
        }
        req(!is.null(data))
        tofilter <- data
        ints <- sapply(tofilter, is.integer)
        integ <-
            apply(tofilter[, ints, drop = F], 2, function(x) {
                #if a column is num but has less than 4 distinct values, turn to ordered factor
                length(unique(x)) <= 100 &&
                    sum(table(x) == 1) < length(unique(x)) / 2
            })
        integerCol <- names(integ)[integ]
        
        if (length(integerCol) > 0) {
            tofilter[, integerCol] <-
                as.data.frame(lapply(tofilter[, integerCol], as.ordered))
        }
        
        req(rv$value_store, rv$group_store)
        
        group <-
            callModule(
                #call the filter shiny module from the esquisse package
                module = filterDF,
                id = "grouping",
                data_table = reactive(tofilter),
                data_name = reactive("data"),
                data_vars = reactive(rv$group_store)
            )
        
        res_filter <-
            callModule(
                #call the filter shiny module from the esquisse package
                module = filterDF,
                id = "filtering",
                data_table = reactive(group$data_filtered()),
                data_name = reactive("to_filter"),
                data_vars = reactive(rv$value_store)
            )
        output$filter_button <-
            renderUI({
                actionButton("filter_button", "Filter Table")
            })
        
        filtered <- eventReactive(input$filter_button, {
            a <- data[-as.numeric(rownames(group$data_filtered())),]
            b <-
                data[as.numeric(rownames(res_filter$data_filtered())),]
            data_filtered <- as.data.frame(rbind(a, b))
            
            return(data_filtered)
        })
        
        
        observeEvent(filtered(), {
            #update the bar with the % of data after filtering
            updateProgressBar(
                session = session,
                id = "pbar",
                value = nrow(filtered()),
                total = nrow(data)
            )
        })
        #render tables, buttons, summaries
        output$table <- DT::renderDT({
            filtered()
        }, options = list(pageLength = 5))
        
        #output$show_filtered_structure <- renderUI({actionButton("show_filtered_structure", "Show filtered data structure")})
        
        #observeEvent(input$show_filtered_structure, {
        output$res_str <- renderPrint({
            str(filtered())
        })
        #})
        
        #output$show_filtered_summary <- renderUI({actionButton("show_filtered_summary", "Show filtered data summary")})
        
        #observeEvent(input$show_filtered_summary, {
        output$res_summary <- renderPrint({
            summary(filtered())
        })
        # })
        
        output$filter_update <-
            renderUI({
                actionButton("filter_update", "Update Table")
            })
        observeEvent(input$filter_update, {
            #button that pressed overwrites the filtered data as the reactive value
            filtered <- filtered()
            req(filtered)
            
            filtered <-
                filtered %>% mutate_if(is.factor, delete_lvl)
            # if(exists("seroconv_type", where = filtered)){
            #     filtered$seroconv_type <- as.factor(as.character(filtered$seroconv_type))
            # }
            if (!is.null(original_data())) {
                req(input$dataset_choice)
                if (input$dataset_choice == "patient data") {
                    datasets$constant_data <- filtered
                    visit <-
                        visit_data()[visit_data()[, input$choose_id] %in% datasets$constant_data[, input$choose_id],]
                    visit <-
                        visit %>% mutate_if(is.factor, delete_lvl)
                    datasets$visit_data <- visit
                } else if (input$dataset_choice == "visit data") {
                    datasets$visit_data <- filtered
                    constant <-
                        constant_data()[constant_data()[, input$choose_id] %in% datasets$visit_data[, input$choose_id],]
                    constant <-
                        constant %>% mutate_if(is.factor, delete_lvl)
                    datasets$constant_data <- constant
                }
            }
        })
    })
    
})

output$filter_reset <-
    renderUI({
        actionButton("filter_reset", "Reset Table")
    })
observeEvent(input$filter_reset, {
    #overwrites the reactive values with the original datasets
    datasets$constant_data <- constant_data()
    datasets$visit_data <- visit_data()
})

#render the saved tables and download buttons
output$test_reactivedata <- DT::renderDT({
    datasets$constant_data
}, options = list(pageLength = 5))

output$downloadFilteredConstantData <-
    downloadHandler(
        filename = function() {
            paste("constant_filtered", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasets$constant_data,
                      file, row.names = FALSE)
        }
    )


output$test_visitdata <- DT::renderDT({
    datasets$visit_data
}, options = list(pageLength = 5))

output$downloadFilteredVisitData <-
    downloadHandler(
        filename = function() {
            paste("visit_filtered", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasets$visit_data,
                      file, row.names = FALSE)
        }
    )
