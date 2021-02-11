#Server module for preparing the data to be visualized (code adapted from https://github.com/htpmod/HTPmod-shinyApp/tree/master/htpdvis)
# suppressWarnings(
#     source('pca.plot.R')
# )
#for determining which columns are numeric and which not etc.
tryObserve <- function(x) {
    x <- substitute(x)
    env <- parent.frame()
    observe({
        tryCatch(
            eval(x, env),
            error = function(e) {
                showNotification(paste("Error: ", e$message), type = "error")
            }
        )
    })
}

prepareData <- function(data) {
    withProgress(value = 0.5,
                 message = "Preparing data",
                 detail = "...",
                 {
                     data <- as.data.frame(data)
                     nums <- sapply(data, is.numeric)
                     if ("All" %in% input$cluster_vars) {
                         numericCol <- colnames(data)[nums]
                     } else{
                         numericCol <- input$cluster_vars
                     }
                     nonNumeric <- nonNumericSize <- NULL
                     # if(any(sapply(data, is.integer))){
                     #     integ <- colnames(data[sapply(data, is.integer)])
                     integ <-
                         apply(data[, nums, drop = F], 2, function(x) {
                             #if a column is num but has less than 4 distinct values, turn to ordered factor
                             length(unique(x)) <= 4 &&
                                 sum(table(x) == 1) < length(unique(x))
                         })
                     integ <- names(integ)[integ]
                     #     data[,integ] <- as.data.frame(lapply(data[,integ] , as.ordered))
                     # }
                     mat <- NULL
                     if (length(numericCol) > 0) {
                         data[, numericCol] <-
                             as.data.frame(sapply(data[, numericCol], as.numeric))
                         mat <- as.data.frame(data[, numericCol])
                         
                     }
                     setProgress(value = 1, detail = "done!")
                 })
    na.test <-  function (x) {
        w <- sapply(x, function(x)
            all(is.na(x)))
        if (any(w)) {
            return(TRUE)
        } else{
            return(FALSE)
        }
    }
    allna <- na.test(mat)
    
    if (input$treatment == "delete rows with NAs") {
        #complete cases if option to delete rows with NA is chosen
        if (sum(is.na(mat)) > 0) {
            mat <- mat[complete.cases(mat),]
            data <- data[rownames(mat), ]
        }
    } else if (input$treatment == "bayesian imputation") {
        #bayesian imputation if chosen
        ## preprocessing: fill NA values
        if (sum(is.na(mat)) > 0) {
            NPC <- max(trunc(sqrt(ncol(mat))), 3)
            withProgress(value = 0.5,
                         message = "Data preprocessing",
                         detail = "Estimating 'NA' values using Bayesian PCA",
                         {
                             if (!is.null(allna)) {
                                 if (allna) {
                                     showNotification(
                                         "Warning: Some columns contain only NA. They will be removed",
                                         duration = NULL,
                                         type = "warning"
                                     )
                                     mat <-
                                         mat[, colSums(is.na(mat)) != nrow(mat)]
                                 }
                                 if (any(!rowSums(!is.na(mat)))) {
                                     mat <- mat[!!rowSums(!is.na(mat)), ]
                                     showNotification(
                                         "Warning: Some rows contain only NA in the numeric variables. They have been removed",
                                         duration = NULL,
                                         type = "warning"
                                     )
                                 }
                                 resBPCA <-
                                     suppressWarnings(pca(
                                         as.matrix(mat),
                                         method = "bpca",
                                         nPcs = NPC
                                     ))
                                 ## Get the estimated complete observations
                                 mat <-
                                     as.data.frame(completeObs(resBPCA))
                                 setProgress(value = 1, detail = "done!")
                             }
                         })
            data <- data[rownames(mat), ]
            
        }
        ## Perform Bayesian PCA with * components
        
    }
    
    if (!is.null(nums)) {
        nonNumeric <- colnames(data)[!nums]
        if (length(nonNumeric) > 0) {
            unicols <-
                apply(data[, c(integ, nonNumeric), drop = F], 2, function(x) {
                    length(unique(x)) != length(x) &&
                        sum(table(x) == 1) < length(unique(x)) / 2 &&
                        length(unique(x)) < 30 #maximum 30 distinct values otherwise app crashes
                })
            nonNumeric <- names(unicols)[unicols]
            
            data[, nonNumeric] <-
                as.data.frame(sapply(data[, nonNumeric] , as.factor))
            
            nonNumericSize <-
                apply(data[, nonNumeric, drop = F], 2, function(x) {
                    length(unique(x))
                })
        } else{
            unicols <- apply(data[, c(integ), drop = F], 2, function(x) {
                length(unique(x)) != length(x) &&
                    sum(table(x) == 1) < length(unique(x)) / 2 &&
                    length(unique(x)) < 30 #maximum 30 distinct values otherwise app crashes
            })
            nonNumeric <- names(unicols)[unicols]
            
            data[, nonNumeric] <-
                as.data.frame(sapply(data[, nonNumeric] , as.factor))
            
            nonNumericSize <-
                apply(data[, nonNumeric, drop = F], 2, function(x) {
                    length(unique(x))
                })
        }
        
    }
    
    nums <- sapply(data, is.numeric)
    numericCol <- colnames(data)[nums]
    
    data <- data %>% mutate_if(is.factor,
                               fct_explicit_na,
                               na_level = "Missing")
    data <- data %>% mutate_if(is.factor, delete_lvl)
    
    
    
    
    #warning levels with one entry
    factors <- data[, nonNumeric]
    facLevel <- lapply(factors, table)
    to_change <- lapply(facLevel, function(x)
        names(x)[x == 1])
    for (i in names(factors)) {
        if (length(to_change[[i]]) == 1) {
            showNotification(
                paste0(
                    "Warning: variable ",
                    as.character(i),
                    " contains only one entry in the group "
                    ,
                    to_change[[i]],
                    ". Consider removing this entry using the data filter."
                ) ,
                duration = NULL,
                type = "warning"
            )
            
        } else if (length(to_change[[i]]) > 1) {
            showNotification(
                paste0(
                    "Warning: variable ",
                    as.character(i),
                    " contains only one entry in the groups "
                    ,
                    paste(to_change[[i]], sep = ", ", collapse = ""),
                    ". Consider removing these entries using the data filter."
                ) ,
                duration = NULL,
                type = "warning"
            )
            
        }
    }
    
    #remove zero variance columns
    removeZeroVar <- function(df) {
        df[,!sapply(df, function(x)
            min(x, na.rm = T) == max(x, na.rm = T))]
    }
    
    allna <- na.test(mat)
    req(!is.null(mat) & !allna)
    if (any(sapply(mat, var, na.rm = T) == 0)) {
        showNotification(
            paste0(
                "Warning: numeric columns: ",
                paste(as.character(names(
                    which(sapply(mat, var, na.rm = T) == 0)
                )), collapse = " ,"),
                " have 0 variance. They will be removed from the analysis"
            ),
            duration = NULL,
            type = "warning"
        )
        
        mat <- removeZeroVar(mat)
    }
    allna <- na.test(mat)
    if (allna) {
        showNotification(
            "numeric data has 0 non-missing values. Change input data or choose different treatment method.",
            duration = NULL,
            type = "error"
        )
    }else if(any(is.na(mat))){
        showNotification(
            "Warning: Your dataset contains missing values! Clustering methods require ONLY complete cases. Consider treating the missing values in your data.",
            duration = NULL,
            type = "error"
        )
    }
    
    attr(data, 'class') <- 'data.frame'
    save(data, file = paste0(mydir, "/data.RData"))
    save(mat, file = paste0(mydir, "/mat.RData"))
    save(numericCol, file = paste0(mydir, "/numericCol.RData"))
    save(nonNumeric, file = paste0(mydir, "/nonNumeric.RData"))
    save(nonNumericSize, file = paste0(mydir, "/nonNumericSize.RData"))
    out <-
        list(
            data = data,
            mat = as.matrix(mat),
            numericCol = numericCol,
            nonNumeric = nonNumeric,
            nonNumericSize = nonNumericSize
        )
    save(out, file = paste0(mydir, "/.RData"))
    return(out)
}

myValidation <- function(inputData) {
    ## showNotification("Checking Data ...");
    withProgress(value = 0.5,
                 message = "Checking Data ",
                 detail = "...",
                 {
                     ## inputData <- inputDataAnalysis()
                     shiny::validate(
                         need(
                             !is.null(input$visualization_data),
                             "Upload your file."
                         ),
                         need(!is.nothing(inputData),
                              "No data detected."),
                         need(
                             nrow(inputData$mat) > 0 && ncol(inputData$mat) > 0,
                             "No numeric data found. Please check your input file."
                         ),
                         need(
                             all(sapply(inputData$mat, is.numeric)),
                             "The matrix 'X' contains non-numeric values."
                         ),
                         need(
                             ncol(inputData$mat) >= 2,
                             "Choose at least 2 numeric variables"
                         )
                     )
                     setProgress(value = 1, detail = "done!")
                 })
    
}

output$choose_vis_vars <-
    renderUI({
        #UI element to choose variables to include in the analysis
        req(input$visualization_data)
        if (input$visualization_data == "patient data") {
            cols <- colnames(datasets$constant_data)
        } else if (input$visualization_data == "visit data") {
            cols <- colnames(datasets$visit_data)
        }
        selectizeInput(
            "choose_vis_vars",
            "Choose variables to visualize: ",
            multiple = T,
            choices = cols,
            selected = NULL
        )
    })
output$choose_cluster_vars <- renderUI({
    req(input$choose_vis_vars)
    if (input$visualization_data == "patient data") {
        cols <-
            colnames(select_if(as.data.frame(datasets$constant_data[, input$choose_vis_vars]), is.numeric))
    } else if (input$visualization_data == "visit data") {
        cols <-
            colnames(select_if(as.data.frame(datasets$visit_data[, input$choose_vis_vars]), is.numeric))
    }
    selectizeInput(
        "cluster_vars",
        "Of which numeric variables for clustering:",
        multiple = T,
        choices = c("All", cols),
        selected = "All"
    )
})


inputDataAnalysis <- eventReactive(input$go, {
    if (is.null(original_data()))
        return(NULL)
    if (input$visualization_data == "patient data") {
        patient <- datasets$constant_data[, input$choose_vis_vars]
        if (!is.nothing(input$choose_vis_vars)) {
            data <- prepareData(patient)
        }
    } else if (input$visualization_data == "visit data") {
        visit <- datasets$visit_data[, input$choose_vis_vars]
        if (!is.nothing(input$choose_vis_vars)) {
            data <- prepareData(visit)
        }
        
    } else{
        data <- NULL
    }
    data
})

observe({
    inputData <- inputDataAnalysis()
    if (is.null(inputData$data)) {
        shinyjs::hide("div_step2")
        shinyjs::hide("div_step3")
    } else {
        shinyjs::show("div_step2")
        shinyjs::show("div_step3")
        withProgress(value = 0.5,
                     message = "Rendering UI. Please wait ...",
                     detail = "...",
                     {
                         output$uiX1 <- renderPrint({
                             input$visualization_data
                             data <- inputData$data
                             dim(data)
                         })
                         output$uiX2 <- renderPrint({
                             input$visualization_data
                             data <- inputData$mat
                             dim(data)
                         })
                         output$uicpRow <- renderUI({
                             choicesRow <- inputData$nonNumeric
                             input$visualization_data
                             div(
                                 selectizeInput(
                                     "rowColor",
                                     "Point colors by: ",
                                     multiple = F,
                                     choices = choicesRow
                                 ),
                                 selectizeInput(
                                     "rowPoint",
                                     "Point symbols by (only for clustering plots): ",
                                     multiple = F,
                                     choices = choicesRow
                                 ),
                                 selectizeInput(
                                     "hover",
                                     "Choose variables to see on hover (only for clustering plots):",
                                     choices = colnames(inputData$data),
                                     multiple = T,
                                     options = list(placeholder = 'Choose variables'),
                                     selected = NULL
                                 )
                             )
                         })
                         setProgress(value = 1, detail = "done!")
                     })
    }
})

output$inputData <-
    DT::renderDataTable({
        #render data table to be analyzed
        if (!is.null(inputDataAnalysis())) {
            inputData <- inputDataAnalysis()
            myValidation(inputData)
            showNotification("Rendering Table ...")
            
            data <- inputData$data
            if (ncol(data) > 4) {
                n <- ncol(data) - 4
                
                data <-
                    cbind(data[, 1:4], More = paste0("... ", n, " columns"))
            }
            data
        }
    }, options = list(pageLength = 5))

output$bayesian <-
    DT::renderDataTable({
        #render numeric table to be analyzed
        if (!is.null(inputDataAnalysis())) {
            inputData <- inputDataAnalysis()
            myValidation(inputData)
            showNotification("Rendering Table ...")
            
            data <- inputData$mat
            if (ncol(data) > 4) {
                n <- ncol(data) - 4
                
                data <-
                    cbind(data[, 1:4], More = paste0("... ", n, " columns"))
            }
            data
        }
    }, options = list(pageLength = 5))

output$downloadInputData <- downloadHandler(
    "visulization_data_file.txt",
    content = function(file) {
        data <- inputDataAnalysis()$data
        write.table(
            data,
            file = file,
            row.names = F,
            quote = F,
            sep = "\t"
        )
    },
    contentType = "text/tsv"
)


#Server module for visualizing/analyzing the data (code adapted from https://github.com/htpmod/HTPmod-shinyApp/tree/master/htpdvis)
uiValidation <- function() {
    need(
        !is.null(input$rowColor) &&
            !is.null(input$rowPoint) &&
            !is.null(input$npcs),
        "Some UIs not yet initiate. "
    )
}
## cat(class(inputDataAnalysis()$data))


getcolor_list <- function(inputData) {
    uiValidation()
    ccol <- input$rowColor
    data <- inputData$data
    if (ccol %in% colnames(data)) {
        cids <- sort(unique(data[, ccol]), na.last = T)
        if (length(cids) <= 4) {
            color_list <-
                c("#FF0000", "#FFB833", "#0000FF" , "#00FF00")[1:length(cids)]
        } else if (length(cids) < 8) {
            color_list <-
                c(
                    "#FF0000",
                    "#FFB833",
                    "#0000FF",
                    "#00FF00",
                    colorRampPalette(brewer.pal(
                        n = 3, name = "Set2"
                    ))(length(cids) - 4)
                )
        } else if (length(cids) < 12) {
            color_list <-
                c(
                    "#FF0000",
                    "#FFB833",
                    "#0000FF",
                    "#00FF00",
                    colorRampPalette(brewer.pal(
                        n = 8, name = "Set2"
                    ))(length(cids) - 4)
                )
        } else{
            color_list <-
                c(
                    "#FF0000",
                    "#FFB833",
                    "#0000FF",
                    "#00FF00",
                    colorRampPalette(brewer.pal(
                        n = 8, name = "Set2"
                    ))(8),
                    rainbow(length(cids) - 12)
                )
            
        }
        names(color_list) <- cids
        color_list
    } else{
        NULL
    }
}


getsymbol_list <- function(inputData) {
    uiValidation()
    cpty <- input$rowPoint
    data <- inputData$data
    vals <- schema(F)$traces$scatter$attributes$marker$symbol$values
    vals <- vals[-which(str_detect(vals, '0|1|2|3|4|5|6|7|8|9'))]
    nodot <- vals[which(str_detect(vals, '-dot', negate = T))]
    opendot <- vals[which(str_detect(vals, 'open-dot'))]
    vals <-
        c(nodot, opendot)#eliminate shapes with dot that are not open cause they don't render properly.
    
    plotly_shapes <-
        c(
            "circle",
            "square",
            "diamond",
            "cross",
            "x",
            "circle-open",
            "square-open",
            "diamond-open",
            "cross-open",
            "x-open",
            "circle-open-dot",
            "square-open-dot",
            "diamond-open-dot",
            "cross-open-dot",
            "x-open-dot"
        )#preferred order of shapes
    shapes <- c(plotly_shapes, vals[!vals %in% plotly_shapes])
    if (cpty %in% colnames(data)) {
        pids <- sort(unique(data[, cpty]), na.last = T)
        symbol_list <- shapes[1:length(pids)]
        names(symbol_list) <- pids
        symbol_list
    } else{
        NULL
    }
}

getggsymbol_list <- function(inputData) {
    uiValidation()
    cpty <- input$rowPoint
    data <- inputData$data
    shapes <-
        c(16,
          15,
          18,
          43,
          88,
          1,
          0,
          5,
          17,
          35,
          42,
          6,
          24,
          37,
          81,
          68,
          48,
          64,
          36,
          94,
          126,
          60:63)
    if (cpty %in% colnames(data)) {
        pids <- sort(unique(data[, cpty]), na.last = T)
        ggsymbol_list <- shapes[1:length(pids)]
        names(ggsymbol_list) <- pids
        ggsymbol_list
    } else{
        NULL
    }
}

params <- reactive({
    need(!is.null(inputDataAnalysis()), "Data is not loaded.")
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    uiValidation()
    symbol_list <- getsymbol_list(inputData)
    #symbols for ggplot
    ggsymbol_list <- getggsymbol_list(inputData)
    color_list <- getcolor_list(inputData)
    mapped_symbols <- mapped_colors <- NA
    if (input$rowPoint != "") {
        name1 <-  inputData$data[, input$rowPoint]
        mapped_symbols <-
            symbol_list[inputData$data[, input$rowPoint]]
    } else{
        symbol_list <- "circle"
        mapped_symbols <- rep(c(All = 'circle'), nrow(inputData$data))
        #symbols for ggplot
        ggsymbol_list <- "circle"
        name1 <- I(symbol_list)
    }
    if (input$rowColor != "") {
        mapped_colors <- color_list[inputData$data[, input$rowColor]]
        name2 <- inputData$data[, input$rowColor]
    } else{
        color_list <- c(All = '#ff7f00')
        mapped_colors <- rep(c(All = '#ff7f00'), nrow(inputData$data))
        name2 <- I(color_list)
    }
    names(mapped_colors) <-
        names(mapped_symbols) <- rownames(inputData$data)
    if (input$rowPoint == "" && input$rowColor != "") {
        name <- paste(input$rowColor, ": ", name2)
    } else if (input$rowPoint != "" && input$rowColor == "") {
        name <- paste(input$rowPoint, ": ", name1)
    } else if (input$rowPoint == "" && input$rowColor == "") {
        name <- "All"
    } else{
        name <-
            paste(input$rowPoint,
                  ": ",
                  name1,
                  "<br>",
                  input$rowColor,
                  ": ",
                  name2)
    }
    
    if (!is.null(input$hover)) {
        text <- NULL
        hoverdata <- inputData$data[, input$hover, drop = FALSE]
        text <-
            t(apply(hoverdata , 1, function(x)
                paste0(
                    colnames(hoverdata), paste(":", as.character(x))
                )))
        if (length(input$hover) > 1) {
            text <- apply(text, 1 , paste , collapse = "<br>")
        }
    } else{
        text <- ""
    }
    text <- as.vector(text)
    params <-
        list(
            text = text,
            symbol_column = name1,
            color_column = name2,
            name = name,
            symbol_list = symbol_list,
            ggsymbol_list = ggsymbol_list,
            color_list = color_list,
            mapped_colors = mapped_colors,
            mapped_symbols = mapped_symbols
        )
    return(params)
})

ggplot <-
    function(...)
        ggplot2::ggplot(...) + scale_color_manual(values = c(
            "#FF0000",
            "#FFB833",
            "#0000FF",
            "#00FF00",
            colorRampPalette(brewer.pal(n = 8, name = "Set2"))(8),
            rainbow(2000)
        )) +
    scale_fill_manual(values = c(
        "#FF0000",
        "#FFB833",
        "#0000FF",
        "#00FF00",
        colorRampPalette(brewer.pal(n = 8, name = "Set2"))(8),
        rainbow(2000)
    ))
unlockBinding("ggplot", parent.env(asNamespace("GGally")))
assign("ggplot", ggplot, parent.env(asNamespace("GGally")))
