output$nmds <- renderUI({
    sliderInput(
        "nmds",
        "Dimension",
        min = 2,
        max = ncol(inputDataAnalysis()$mat),
        value = ifelse(ncol(inputDataAnalysis()$mat) < 3, ncol(inputDataAnalysis()$mat), 3),
        step = 1
    )
})

MDS_analysis <- reactive({
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    req(input$nmds)
    req(input$dist_method)
    ndims <- input$nmds
    withProgress(
        value = 0,
        message = paste("Performing MDS with N =", ndims),
        detail = "...",
        {
            mat <-
                apply(inputData$mat, 2, function(x) {
                    scale(x,
                          scale = input$scale,
                          center = input$center)
                })
            ## all_norm <- normalize.quantiles(as.matrix(inputData$mat))
            ## mat <- t(apply(all_norm, 1, function(x){scale(x, scale=input$scale, center=input$center)}))
            dmth <- input$dist_method
            if (dmth == "") {
                dmth <- "euclidean"
            }
            MDS_out <-
                cmdscale(dist(mat, method = dmth), k = ndims)
            colnames(MDS_out) <-
                paste0("MDS", 1:ncol(MDS_out))
            
            setProgress(value = 0.5, detail = "preparing data ...")
            return(MDS_out)
            setProgress(value = 1, detail = "done!")
            
        }
    )
})


MDS_plotly <- reactive({
    need(!is.null(MDS_analysis()),
         message = "MDS not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    req(input$nmds)
    ndims <- input$nmds
    MDS_out <- MDS_analysis()
    inputData <- inputDataAnalysis()
    symbol_list <- params()$symbol_list
    color_list <- params()$color_list
    mapped_colors <- params()$mapped_colors
    mapped_symbols <- params()$mapped_symbols
    symbol <- params()$symbol_column
    color <- params()$color_column
    name <- params()$name
    text <- params()$text
 
    
    withProgress(
        value = 0,
        message = paste("Making MDS graph"),
        detail = "...",
        {
            pltd <- as.data.frame(MDS_out)
            if (ncol(pltd) > 2) {
                t <-
                    plot_ly(
                        x = pltd[, 1],
                        y = pltd[, 2],
                        z = pltd[, 3],
                        type = "scatter3d",
                        mode = "markers",
                        symbol = symbol,
                        symbols = rep_len(symbol_list[1:6], length(unique(symbol))),
                        #c("circle","square","diamond", "cross","x","circle-open","square-open","diamond-open"),
                        color = color,
                        colors = color_list,
                        #name = name,
                        text = paste(name, "<br>", text),
                        marker = list(
                            size = 5,
                            opacity = 0.8,
                            line = list(width = 2)
                        ),
                        showlegend = T
                    ) %>%
                    layout(scene = list(
                        xaxis = list(title = 'MDS1'),
                        yaxis = list(title = 'MDS2'),
                        zaxis = list(title = 'MDS3')
                    ))
                if (length(unique(symbol)) > 6) {
                    showNotification(
                        "3D scatter plot only takes 6 symbols. Your data has more than 6 categories. 3D plot will repeat the point symbols."
                    )
                }
                dimensions <- list()
                for (i in 1:ncol(pltd)) {
                    dimensions[[i]] <- list(label = paste0('MDS', i),
                                            values = pltd[, i])
                }
                axis = list(
                    showline = FALSE,
                    zeroline = FALSE,
                    gridcolor = '#ffff',
                    ticklen = 4
                )
                pairdata <- pltd
                pairdata$color <- mapped_colors
                pairdata$symbol <- mapped_symbols
                
                pairMDS <- pairdata %>% plot_ly(
                    type = 'splom',
                    dimensions = dimensions,
                    marker = list(
                        symbol = ~ symbol,
                        size = 8,
                        opacity = 0.8,
                        color = ~ color,
                        line = list(
                            color = ~ color,
                            opacity = 1,
                            width = 2
                        )
                    ),
                    text = paste(name, "<br>", text),
                    showlegend = T
                )
                
                pairMDS <- pairMDS %>%
                    layout(
                        #legendgroup= name,
                        showlegend = F,
                        title = 'MDS',
                        hovermode = 'closest',
                        dragmode = 'select'
                    )
                pairMDS <-
                    pairMDS %>% style(diagonal = list(visible = F))
                pairMDS <-
                    pairMDS %>% style(showupperhalf = F)
                
                
                
                MDS_plots <-
                    list(tridplot = t,
                         pairMDS = pairMDS,
                         scores = pltd)
                
            } else if (ncol(pltd) == 2) {
                pairMDS <-
                    plot_ly(
                        x = pltd[, 1],
                        y = pltd[, 2],
                        type = "scatter",
                        mode = "markers",
                        symbol = symbol,
                        #I(symbol_list),#
                        symbols = symbol_list,
                        #c("circle","square","diamond", "cross","x","circle-open","square-open","diamond-open"),
                        color = color,
                        #I(color_list),
                        colors = color_list,
                        #name = name,
                        text = paste(name, "<br>", text),
                        marker = list(
                            size = 8,
                            opacity = 0.8,
                            line = list(opacity = 1,
                                        width = 2)
                        ),
                        showlegend = T
                    ) %>%
                    layout(
                        title = "MDS",
                        xaxis = list(title = "MDS1",
                                     zeroline = FALSE),
                        yaxis = list(title = "MDS2",
                                     zeroline = FALSE)
                    )
                MDS_plots <-
                    list(tridplot = pairMDS,
                         pairMDS = pairMDS,
                         scores = pltd)
                
            } else{
                pairMDS <- plot_ly(
                    x = pltd[, 1],
                    type = "scatter",
                    mode = "markers",
                    symbol = symbol,
                    #I(symbol_list),#
                    symbols = symbol_list,
                    #c("circle","square","diamond", "cross","x","circle-open","square-open","diamond-open"),
                    color = color,
                    #I(color_list),
                    colors = color_list,
                    #name = name,
                    text = paste(name, "<br>", text),
                    marker = list(
                        size = 8,
                        opacity = 0.8,
                        line = list(opacity = 1,
                                    width = 2)
                    ),
                    showlegend = T
                ) %>%
                    layout(
                        title = "MDS",
                        xaxis = list(title = "MDS1",
                                     zeroline = FALSE),
                        yaxis = list(title = "Index",
                                     zeroline = FALSE)
                    )
                
                MDS_plots <-
                    list(tridplot = pairMDS,
                         pairMDS = pairMDS,
                         scores = pltd)
                
            }
            setProgress(value = 1, detail = "done!")
            
            return(MDS_plots)
        }
    )
    
})

output$MDS_tridplot <- renderPlotly({
    MDS_plotly()$tridplot
})

output$MDS_pairplot <- renderPlotly({
    MDS_plotly()$pairMDS
})

output$downloadMdsScores <- downloadHandler(
    "MDS_scores.txt",
    content = function(file) {
        data <- MDS_plotly()$scores
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