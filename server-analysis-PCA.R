


PCA_analysis <- reactive({
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    withProgress(
        value = 0,
        message = paste0("Performing PCA"),
        detail = "...",
        {
            pca.dudi <-
                dudi.pca(
                    inputData$mat,
                    center = input$center,
                    scale = input$scale,
                    scan = FALSE,
                    nf = ncol(inputData$mat)
                )
            
            setProgress(value = 0.5, detail = "preparing data ...")
            return(pca.dudi)
            setProgress(value = 1, detail = "done!")
            
        }
    )
})

output$npcs <- renderUI({
    sliderInput(
        "npcs",
        "Number of components",
        min = 1,
        max = ncol(inputDataAnalysis()$mat),
        value = ifelse(ncol(inputDataAnalysis()$mat) < 3, ncol(inputDataAnalysis()$mat), 3),
        step = 1
    )
})


PCA_plotly <- reactive({
    need(!is.null(PCA_analysis()),
         message = "PCA not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    req(input$npcs)
    nPCs <- input$npcs
    pca.dudi <- PCA_analysis()
    R2 <- pca.dudi$eig / sum(pca.dudi$eig) * 100
    loading <- pca.dudi$co
    colnames(loading) <- paste0("PC", 1:ncol(loading))
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
        message = paste0("Rendering plot"),
        detail = "...",
        {
            setProgress(value = 0.5, detail = "generating plot ...")
            pltd <- t(loading)
            suppressWarnings(
                heatmap <-
                    plot_ly(
                        x = colnames(pltd[1:nPCs,]),
                        y = rownames(pltd[1:nPCs, ]),
                        z = pltd[1:nPCs, ],
                        type = "heatmap"
                    ) %>%
                    colorbar(title = "loading value") %>%
                    layout(
                        title = "Loading score Heatmap",
                        xaxis = list(title = "Features")
                    )
            )
            
            
            col1 <- rep("not plotted PC", length(R2))
            col1[1:nPCs] <- "Plotted PC"
            names(col1) <- 1:length(col1)
            col1 <- as.factor(col1)
            
            variance <-
                ggplot(data = data.frame(x = c(1:length(R2)), y = R2), aes(x = x, y = y)) +
                geom_segment(aes(
                    x = x,
                    xend = x,
                    y = 0,
                    yend = y,
                    color = col1
                )) +
                geom_point(
                    aes(color = col1),
                    size = 5,
                    alpha = 0.8,
                    shape = "circle",
                    stroke = 2
                ) +
                geom_line() +
                scale_color_manual(values = c("Plotted PC" = "red", "not plotted PC" =
                                                  "gray")) +
                xlab("PC") +
                ylab("% variance") +
                labs(title = "Explained variance",
                     subtitle = "eigenvalues/sum(eigenvalues)*100")
            
            variance <- ggplotly(variance)
            
            pltd <- pca.dudi$li
            
            if (nPCs > 2) {
                suppressWarnings(
                    p <-
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
                            xaxis = list(title = 'PC1'),
                            yaxis = list(title = 'PC2'),
                            zaxis = list(title = 'PC3')
                        ))
                    #scatterplot3d(pltd[,1],pltd[,2],pltd[,3], color=mapped_colors, pch = mapped_symbols, angle = input$angle )
                )
                if (length(unique(symbol)) > 6) {
                    showNotification(
                        "3D scatter plot only takes 6 symbols. Your data has more than 6 categories. 3D plot will repeat the point symbols."
                    )
                }
                dimensions <- list()
                for (i in 1:ncol(pltd[, 1:nPCs])) {
                    dimensions[[i]] <- list(label = paste0('PC', i),
                                            values = pltd[, i])
                }
                axis = list(
                    showline = FALSE,
                    zeroline = FALSE,
                    gridcolor = '#ffff',
                    ticklen = 4
                )
                pairdata <- pltd[, 1:nPCs]
                pairdata$color <- mapped_colors
                pairdata$symbol <- mapped_symbols
                
                pairpc <- pairdata %>% plot_ly(
                    #name = name,
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
                # pairpc <- pairpc %>%
                #     add_trace(
                #
                #     )
                pairpc <- pairpc %>%
                    layout(
                        showlegend = F,
                        title = 'PCA',
                        hovermode = 'closest',
                        dragmode = 'select'
                    )
                pairpc <-
                    pairpc %>% style(diagonal = list(visible = F))
                pairpc <-  pairpc %>% style(showupperhalf = F)
                
                
                
                pca_plots <-
                    list(
                        tridplot = p,
                        heatmap = heatmap,
                        pairpc = pairpc,
                        variance = variance
                    )
                
                
            } else if (nPCs == 2) {
                suppressWarnings(
                    pairpc <-
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
                            title = "PCA",
                            xaxis = list(title = "PC1",
                                         zeroline = FALSE),
                            yaxis = list(title = "PC2",
                                         zeroline = FALSE)
                        )
                    #scatterplot3d(pltd[,1],pltd[,2],pltd[,3], color=mapped_colors, pch = mapped_symbols, angle = input$angle )
                )
                #showNotification("3D plot need 3 Pricipal components. Showing heatmap istead.", duration = NULL, type = "warning")
                pca_plots <-
                    list(
                        tridplot = pairpc,
                        heatmap = heatmap,
                        pairpc = pairpc,
                        variance = variance
                    )
                
            } else{
                pairpc <- plot_ly(
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
                        title = "PCA",
                        xaxis = list(title = "PC1",
                                     zeroline = FALSE),
                        yaxis = list(title = "Index",
                                     zeroline = FALSE)
                    )
                
                pca_plots <-
                    list(
                        tridplot = pairpc,
                        heatmap = heatmap,
                        pairpc = pairpc,
                        variance = variance
                    )
                
            }
            setProgress(value = 1, detail = "done!")
        }
    )
    return(
        list(
            pca_plots = pca_plots,
            R2 = data.frame(PC = 1:length(R2), R2 = R2),
            loading = data.frame(Feature = rownames(loading), loading),
            scores = as.data.frame(pca.dudi$li)[, 1:nPCs]
        )
    )
    
})

output$PCA_trdplot <- renderPlotly({
    PCA_plotly()$pca_plots$tridplot
})

output$PCA_pairplot <- renderPlotly({
    PCA_plotly()$pca_plots$pairpc
})

output$PCA_variance <- renderPlotly({
    PCA_plotly()$pca_plots$variance
})
output$PCA_heatmap <- renderPlotly({
    PCA_plotly()$pca_plots$heatmap
})

output$downloadPpcaVar <- downloadHandler(
    "PCA_para_explained_variance.txt",
    content = function(file) {
        data <- PCA_plotly()$R2
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

output$downloadPpcaLoading <- downloadHandler(
    "PCA_para_loading_score.txt",
    content = function(file) {
        data <- PCA_plotly()$loading
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

output$downloadPpcaScores <- downloadHandler(
    "PCA_scores.txt",
    content = function(file) {
        data <- PCA_plotly()$scores
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