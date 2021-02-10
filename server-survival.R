output$surv_func_to_apply <- renderUI({
    req(input$survevent)
    if (!is.nothing(input$survevent)) {
        data <- prep_data()
        selectedvar <- as.factor(data[, c(input$survevent)])
        selectizeInput(
            "surv_func_to_apply",
            label = "choose level to mark event",
            choices = levels(selectedvar),
            multiple = TRUE
        )
    }
})


output$survivalparams <- renderUI({
    req(prep_data())
    cols1 <- names(cbind(select_if(prep_data(), is.numeric)))
    cols2 <- names(cbind(select_if(prep_data(), is.Date)))
    cols3 <- names(cbind(select_if(prep_data(), is.ordered)))
    cols4 <- names(cbind(select_if(prep_data(), is.POSIXct)))
    cols5 <-
        names(cbind(select_if(prep_data(), function(col)
            is.integer(col) &
                length(unique(col)) <= 12)))#12 is no of colors in the Paired palette
    cols <- c(cols1, cols2, cols3, cols4, cols5)
    
    factors <- names(cbind(select_if(prep_data(), is.factor)))
    
    tagList(
        selectizeInput(
            "survtime",
            "survival time variable: ",
            multiple = F,
            choices = cols
        ),
        selectizeInput(
            "survevent",
            "event variable: ",
            multiple = F,
            choices = c(factors, cols5)
        ),
        selectizeInput(
            "survgroup",
            "Group by",
            multiple = F,
            choices = c(factors, cols5)
        )
    )
})

output$survlayer <- renderUI({
    req(prep_data(), input$survtime, input$survevent)
    num <- names(cbind(select_if(prep_data(), is.numeric)))
    int <- names(cbind(select_if(prep_data(), is.integer)))
    factors <- names(cbind(select_if(prep_data(), is.factor)))
    choices <- c("", num, int, factors)
    if (!is.nothing(input$survgroup)) {
        choices <-
            choices[!choices %in% c(input$survtime, input$survevent, input$survgroup)]
    } else{
        choices <- choices[!choices %in% c(input$survtime, input$survevent)]
    }
    if (input$checklayer) {
        tagList(
            selectizeInput(
                "survlayer",
                "Covariates: ",
                multiple = T,
                choices = choices,
                selected = ""
            )
            #req(input$survlayer)
            
        )
    }
    
})
output$layer <- renderUI({
    req(input$survlayer, prep_data())
    if (!is.nothing(input$survlayer)) {
        data <- prep_data()
        data %>% mutate_if(is.factor,
                           fct_explicit_na,
                           na_level = "Missing")
        
        data <- data.frame(data[, c(input$survlayer)])
        if (length(input$survlayer) == 1) {
            colnames(data) <- input$survlayer
        }
        userinput <- list()
        userinput2 <- list()
        
        if (length(data) > 0) {
            factorcols <- cbind(select_if(data, function(col)
                is.factor(col)))
            numcols <-
                cbind(select_if(data, function(col)
                    is.numeric(col)))
            if (length(factorcols) > 0) {
                for (i in colnames(factorcols)) {
                    userinput[[i]] <-
                        selectizeInput(
                            paste("surv_levels_", as.character(i), sep = ""),
                            label = paste("choose values", i),
                            choices = levels(factorcols[, i]),
                            multiple = FALSE,
                            selected = levels(factorcols[, i])[1]
                        )
                }
            }
            
            if (length(numcols) > 0) {
                for (i in colnames(numcols)) {
                    userinput2[[i]] <-
                        sliderInput(
                            inputId = paste("surv_nums_", as.character(i), sep = ""),
                            label = paste("choose value", i),
                            min = min(numcols[, i], na.rm = TRUE),
                            max = max(numcols[, i], na.rm = TRUE),
                            value =  min(numcols[, i], na.rm = TRUE)
                        )
                }
            }
            
            return(c(userinput, userinput2))
        }
        
    }
})

prep_data <- reactive({
    req(datasets$constant_data)
    data <- datasets$constant_data
    data <- as.data.frame(data)
    nums <- sapply(data, is.numeric)
    numericCol <- colnames(data)[nums]
    nonNumeric <- nonNumericSize <- NULL
    # if(any(sapply(data, is.integer))){
    #     integ <- colnames(data[sapply(data, is.integer)])
    integ <-
        apply(data[, nums, drop = F], 2, function(x) {
            #if a column is num but has less than 4 distinct values, turn to ordered factor
            length(unique(x)) <= 12
        })
    integ <- names(integ)[integ]

    if (!is.null(nums)) {
        nonNumeric <- colnames(data)[!nums]
        if (length(nonNumeric) > 0) {
            unicols <-
                apply(data[, c(integ, nonNumeric), drop = F], 2, function(x) {
                    length(unique(x)) != length(x) &&
                        length(unique(x)) < 12 #maximum 12 distinct values otherwise app crashes
                })
            nonNumeric <- names(unicols)[unicols]
            
            data[, nonNumeric] <-
                as.data.frame(lapply(data[, nonNumeric] , as.factor))
            
        } else{
            unicols <- apply(data[, c(integ), drop = F], 2, function(x) {
                length(unique(x)) != length(x) &&
                    length(unique(x)) < 12 #maximum 12 distinct values otherwise app crashes
            })
            nonNumeric <- names(unicols)[unicols]
            
            data[, nonNumeric] <-
                as.data.frame(lapply(data[, nonNumeric] , as.factor))
            
        }
        
    }
    
    data <- data %>% mutate_if(is.factor,
                               fct_explicit_na,
                               na_level = "Missing")
    
    
    
    return(data)
})

data_for_cox <- reactive({
    req(prep_data(),
        input$survevent,
        input$survtime,
        input$survgroup,
    )
    
    data <- prep_data()
    data$newcol <-
        ifelse(data[, input$survevent] %in% input$surv_func_to_apply, 1, 0)
    newcol_name <- paste0("event_", input$survevent, sep = "")
    colnames(data)[colnames(data) == "newcol"] <- newcol_name
    data[, newcol_name] <- as.numeric(data[, newcol_name])
    
    nd <- NULL
    formula <- NULL
    model <- NULL
    #observeEvent(input$survlayer,{
    if (!is.nothing(input$survlayer)) {
        condition <- NULL
        for (i in 1:length(input$survlayer)) {
            condition[i] <- !is.nothing(levels(data[, input$survlayer[i]])) ||
                !is.nothing(input[[paste("surv_nums_", input$survlayer[i], sep = "")]])
        }
        
        if (all(condition)) {
            fact_covs <- NULL
            num_covs <- NULL
            fact_covs <-
                as.character(colnames(cbind(
                    select_if(as.data.frame(data[, input$survlayer, drop = FALSE]),
                              function(col)
                                  is.factor(col))
                )))
            num_covs <-
                as.character(colnames(cbind(
                    select_if(as.data.frame(data[, input$survlayer, drop = FALSE]),
                              function(col)
                                  is.numeric(col))
                )))
            
            factvals <- NULL
            numvals <- NULL
            
            if (!is.nothing(fact_covs)) {
                for (i in fact_covs) {
                    factvals[i] <-
                        input[[paste("surv_levels_", as.character(i), sep = "")]]
                }
            }
            if (!is.nothing(num_covs)) {
                for (i in num_covs) {
                    numvals[i] <- input[[paste("surv_nums_", as.character(i), sep = "")]]
                }
            }
            
            formula = as.formula(
                paste0(
                    "Surv(",
                    input$survtime,
                    ",",
                    newcol_name,
                    ")~",
                    input$survgroup,
                    "+",
                    paste(input$survlayer, collapse = "+"),
                    sep = ""
                )
            )
            #to assess the effect a variable has on the survval curve, we need to create new data.
            if (!is.nothing(numvals) && is.nothing(factvals)) {
                nd <- NULL
                nd <-
                    data.frame(lapply(c(numvals), as.numeric), stringsAsFactors = FALSE)
                nd <-
                    nd[rep(1:nrow(nd), length(levels(as.factor(
                        data[, input$survgroup]
                    )))), ]
                nd <- as.data.frame(nd)
                colnames(nd) <- num_covs
            } else if (!is.nothing(factvals) && is.nothing(numvals)) {
                nd <- NULL
                nd <-
                    data.frame(lapply(c(factvals), as.factor), stringsAsFactors = FALSE)
                nd <-
                    nd[rep(1:nrow(nd), length(levels(as.factor(
                        data[, input$survgroup]
                    )))), ]
                nd <- as.data.frame(nd)
                colnames(nd) <- fact_covs
            } else{
                nd <- NULL
                nd1 <- NULL
                nd2 <- NULL
                nd1 <-
                    data.frame(lapply(c(numvals), as.numeric), stringsAsFactors = FALSE)
                nd1 <-
                    nd1[rep(1:nrow(nd1), length(levels(as.factor(
                        data[, input$survgroup]
                    )))), ]
                nd1 <- as.data.frame(nd1)
                nd2 <-
                    data.frame(lapply(c(factvals), as.factor), stringsAsFactors = FALSE)
                nd2 <-
                    nd2[rep(1:nrow(nd2), length(levels(as.factor(
                        data[, input$survgroup]
                    )))), ]
                nd2 <- as.data.frame(nd2)
                colnames(nd1) <- num_covs
                colnames(nd2) <- fact_covs
                nd <- cbind(nd1, nd2)
            }
            
            
            nd[, input$survgroup] <-
                as.factor(as.character(levels(as.factor(data[, input$survgroup]))))
            
            model = coxph(formula, data = data)
        }
    } else{
        formula = as.formula(
            paste0(
                "Surv(",
                input$survtime,
                ",",
                newcol_name,
                ")~",
                input$survgroup,
                sep = ""
            )
        )
        nd = data.frame(as.factor(as.character(levels(
            as.factor(data[, input$survgroup])
        ))))
        colnames(nd) <- input$survgroup
        model = coxph(formula,
                      data = data,
                      control = coxph.control(eps = 2.22e-308, iter.max = 100))
        
    }
    return(list(
        model = model,
        nd = nd,
        newcol_name = newcol_name,
        data = data
    ))
})
observeEvent(input$plot_survival, {
    output$plot_predicted <- renderPlot({
        req(data_for_cox())
        data <- data_for_cox()$data
        model <- data_for_cox()$model
        nd <- data_for_cox()$nd
        newcol_name <- data_for_cox()$newcol_name
        
        if (!is.nothing(model) & !is.nothing(nd)) {
            fit <-
                do.call(survfit, args = list(model, data = data, newdata = nd))
            
            newplot <-
                ggsurvplot(
                    fit = fit,
                    data = nd,
                    palette = "Paired",
                    risk.table = FALSE,
                    conf.int = TRUE,
                    legend.labs = as.character(levels(data[, input$survgroup]))
                )
            riskformula <-
                as.formula(
                    paste0(
                        "Surv(",
                        input$survtime,
                        ",",
                        newcol_name,
                        ")~",
                        input$survgroup,
                        sep = ""
                    )
                )
            #workaround for risk table
            km.fit <-
                do.call(survfit, args = list(riskformula, data = data))
            km.surv <-
                ggsurvplot(
                    km.fit,
                    data = data,
                    risk.table = TRUE,
                    palette = "Paired"
                )
            
            newplot$table <- km.surv$table
            print(newplot, risk.table.height = 0.5)
        } else{
            NULL
        }
    })
    
})

output$model_summary <- renderPrint({
    summary(data_for_cox()$model)
})

output$forest_plot <- renderPlot({
    req(!is.null(data_for_cox()$model) &&
            length(data_for_cox()$model) != 0)
    tryCatch({
        ggforest(data_for_cox()$model, data = prep_data())
    },
    warning = function(cond) {
        showNotification(
            "Model did not converge. Consider using the filter option to eliminate groups for which the model gives infinite coefficients",
            duration = NULL,
            type = "error"
        )
        # message(cond)
        
    })
    
})