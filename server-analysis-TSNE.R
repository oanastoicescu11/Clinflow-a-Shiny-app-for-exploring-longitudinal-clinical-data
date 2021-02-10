output$ndims <- renderUI({
    sliderInput(
        "ndims",
        "Number of components",
        min = 1,
        max = 3,
        value = ifelse(ncol(inputDataAnalysis()$mat) < 3, ncol(inputDataAnalysis()$mat), 3),
        step = 1
    )
})

TSNE_analysis <- reactive({
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    req(input$ndims)
    ndims <- input$ndims
    tsne_out <- NULL
    withProgress(
        value = 0,
        message = paste("Performing t-SNE with N =", ndims),
        detail = "...",
        {
            perplexity <- 30
            if (nrow(inputData$mat) < 100) {
                perplexity <- ceiling(nrow(inputData$mat) / 4)
            }
            tsne_out <-
                Rtsne(
                    inputData$mat,
                    dims = ndims,
                    perplexity = perplexity,
                    check_duplicates = F,
                    verbose = F,
                    max_iter = 500,
                    pca_center = input$center,
                    pca_scale = input$scale
                )
            colnames(tsne_out$Y) <-
                paste0("t-SNE", 1:ncol(tsne_out$Y))
            setProgress(value = 0.5, detail = "preparing data ...")
            return(tsne_out)
            setProgress(value = 1, detail = "done!")
            
        }
    )
})

TSNE_plotly <- reactive({
    need(!is.null(TSNE_analysis()),
         message = "TSNE not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    req(input$ndims)
    ndims <- input$ndims
    tsne_out <- TSNE_analysis()
    inputData <- inputDataAnalysis()
    symbol_list <- params()$symbol_list
    color_list <- params()$color_list
    mapped_colors <- params()$mapped_colors
    mapped_symbols <- params()$mapped_symbols
    symbol <- params()$symbol_column
    color <- params()$color_column
    name <- params()$name
    text <- params()$text
    
    # if(!is.null(input$hover)){
    #     text <- NULL
    #         hoverdata <- inputData$data[ ,input$hover, drop = FALSE]
    #         text <- t(apply(hoverdata ,1, function(x) paste0(colnames(hoverdata),paste(":", as.character(x)))))
    #         if(length(input$hover) > 1){
    #             text <- apply( text, 1 , paste , collapse = ", " )
    #         }
    #     }else{
    #         text <- name
    #     }
    
    withProgress(
        value = 0,
        message = paste("Making TSNE graph"),
        detail = "...",
        {
            pltd <- as.data.frame(tsne_out$Y)
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
                        xaxis = list(title = 't-SNE1'),
                        yaxis = list(title = 't-SNE2'),
                        zaxis = list(title = 't-SNE3')
                    ))
                if (length(unique(symbol)) > 6) {
                    showNotification(
                        "3D scatter plot only takes 6 symbols. Your data has more than 6 categories. 3D plot will repeat the point symbols."
                    )
                }
                dimensions <- list()
                for (i in 1:ncol(pltd[, 1:ndims])) {
                    dimensions[[i]] <- list(label = paste0('t-SNE', i),
                                            values = pltd[, i])
                }
                axis = list(
                    showline = FALSE,
                    zeroline = FALSE,
                    gridcolor = '#ffff',
                    ticklen = 4
                )
                pairdata <- pltd[, 1:ndims]
                pairdata$color <- mapped_colors
                pairdata$symbol <- mapped_symbols
                pairtsne <-
                    pairdata %>% plot_ly(
                        #name = name,
                        type = 'splom',
                        dimensions = dimensions,
                        marker = list(
                            symbol = ~ symbol,
                            #symbols = symbol_list,
                            size = 8,
                            opacity = 0.8,
                            #c("circle","square","diamond", "cross","x","circle-open","square-open","diamond-open"),
                            color = ~
                                color,
                            #colorscale = color_list,
                            line = list(
                                color = ~ color,
                                opacity = 1,
                                width = 2
                            )
                        ),
                        text = paste(name, "<br>", text)
                    )
                
                pairtsne <- pairtsne %>%
                    layout(
                        showlegend = F,
                        title = 'tSNE',
                        hovermode = 'closest',
                        dragmode = 'select'
                    )
                pairtsne <-
                    pairtsne %>% style(diagonal = list(visible = F))
                pairtsne <-
                    pairtsne %>% style(showupperhalf = F)
                
                
                
                tsne_plots <-
                    list(
                        tridplot = t,
                        pairtsne = pairtsne,
                        scores = pltd
                    )
                
            } else if (ncol(pltd) == 2) {
                pairtsne <-
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
                        title = "t-SNE",
                        xaxis = list(title = "t-SNE1",
                                     zeroline = FALSE),
                        yaxis = list(title = "t-SNE2",
                                     zeroline = FALSE)
                    )
                tsne_plots <-
                    list(
                        tridplot = pairtsne,
                        pairtsne = pairtsne,
                        scores = pltd
                    )
                
            } else{
                pairtsne <- plot_ly(
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
                        title = "t-SNE",
                        xaxis = list(title = "t-SNE1",
                                     zeroline = FALSE),
                        yaxis = list(title = "Index",
                                     zeroline = FALSE)
                    )
                
                tsne_plots <-
                    list(
                        tridplot = pairtsne,
                        pairtsne = pairtsne,
                        scores = pltd
                    )
                
            }
            setProgress(value = 1, detail = "done!")
            
            return(tsne_plots)
        }
    )
    
})

output$TSNE_tridplot <- renderPlotly({
    TSNE_plotly()$tridplot
})

output$TSNE_pairplot <- renderPlotly({
    TSNE_plotly()$pairtsne
})

output$downloadTsneScores <- downloadHandler(
    "TSNE_scores.txt",
    content = function(file) {
        data <- TSNE_plotly()$scores
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