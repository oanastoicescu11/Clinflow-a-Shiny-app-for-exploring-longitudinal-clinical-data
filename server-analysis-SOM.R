

SOM_analysis <- reactive({
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    req(input$xdim, input$ydim, input$topo)
    xDim <- input$xdim
    yDim <- input$ydim
    withProgress(
        value = 0,
        message = paste("Performing SOM"),
        detail = "...",
        {
            mat <-
                apply(inputData$mat, 2, function(x) {
                    scale(x,
                          scale = input$scale,
                          center = input$center)
                })
            
            colnames(mat) <- colnames(inputData$mat)
            rownames(mat) <- rownames(inputData$mat)
            somobj <- list(profile = mat)
            set.seed(123456)
            somobj.supersom <-
                supersom(
                    somobj,
                    somgrid(xDim, yDim, input$topo),
                    whatmap = 1,
                    alpha = c(0.05, 0.01),
                    keep.data = TRUE
                )
            setProgress(value = 0.5, detail = "preparing data ...")
            return(somobj.supersom)
            setProgress(value = 1, detail = "done!")
            
        }
    )
})

SOM_graph <- reactive({
    need(!is.null(SOM_analysis()),
         message = "SOM not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    somobj.supersom <- SOM_analysis()
    inputData <- inputDataAnalysis()
    symbol_list <- params()$symbol_list
    color_list <- params()$color_list
    ggsymbol_list <- params()$ggsymbol_list
    
    
    symbol <- params()$symbol_column
    color <- params()$color_column
    name <- params()$name
    text <- params()$text
    
    
    
    som_grid <- somobj.supersom[[4]]$pts %>%
        as_tibble %>%
        mutate(id = row_number())
    
    som_pts <- tibble(id = somobj.supersom[[2]],
                      dist = somobj.supersom[[3]])
    som_pts <- som_pts %>% left_join(som_grid, by = "id")
    
    
    ndist <- unit.distances(somobj.supersom$grid)
    cddist <-
        as.matrix(object.distances(somobj.supersom, type = "codes"))
    cddist[abs(ndist - 1) > .001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)
    
    som_grid <- som_grid %>% mutate(dist = neigh.dists)
    
    p_dist <- som_grid %>%
        ggplot(aes(x0 = x, y0 = y)) + ggforce::geom_circle(aes(r = 0.5, fill =
                                                                   dist)) +
        scale_fill_gradient(low = "white",
                            high = "gray25",
                            name = "Distance") +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "right"
        )
    
    p <- som_grid %>%
        ggplot(aes(x0 = x, y0 = y)) +
        geom_circle(aes(r = 0.5)) +
        theme(
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        )
    
    # color <- name2
    # symbol <- name1
    point_dist <-
        p_dist + geom_point_interactive(
            data = som_pts,
            position = "jitter",
            aes(
                x,
                y,
                col = color,
                pch = symbol,
                #text = paste(color, symbol,text),
                tooltip = paste(name, "<br>", text),
                data_id = paste(name, "<br>", text),
            ),
            size = 2
        ) +
        theme(
            legend.position = "right",
            legend.justification = "right",
            legend.margin = margin(
                t = 0,
                r = 0,
                l = 0,
                unit = "pt"
            ),
            legend.key.height = unit(10, "pt"),
            legend.key.width = unit(2, "pt")
        )
    if (length(color) > 1) {
        point_dist <- point_dist +
            scale_color_manual(values = color_list, name = input$rowColor)
    }
    if (length(symbol) > 1) {
        point_dist <-
            point_dist + scale_shape_manual(values = ggsymbol_list, name = input$rowPoint)
        
    }
    
    
    som_pts$color <- color
    pts <- som_pts %>%
        group_by(id, x, y) %>%
        dplyr::count(color) %>%
        ungroup() %>%
        dplyr::filter(!is.na(color))
    
    pies <-
        p + geom_arc_bar(
            data = pts,
            aes(
                x0 = x,
                y0 = y,
                r0 = 0,
                r = 0.5,
                amount = n,
                fill = color
            ),
            stat = 'pie'
        )
    if (length(color) > 1) {
        pies <-
            pies + scale_fill_manual(values = color_list, name = input$rowColor)
        
    }
    
    #PCA with SOM clustering
    need(!is.null(PCA_analysis()),
         message = "PCA not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    pcaobj <- PCA_analysis()
    pca_score <- pcaobj$li
    pca_labs <-
        paste("PC",
              1:pcaobj$nf,
              " (",
              round(pcaobj$eig / sum(pcaobj$eig) * 100, 2)[1:pcaobj$nf],
              "%)",
              sep = "")
    colnames(pca_score) <- paste("PC", 1:pcaobj$nf, sep = "")
    cluster.labs <- somobj.supersom$unit.classif
    som.counts <- table(cluster.labs)
    som.labels <-
        paste0("Cluster ", names(som.counts), "\n(", som.counts, ")")
    nc <- length(som.counts)
    if (nc > 8) {
        som.cols <- colorRampPalette(brewer.pal(n = 8, name = "Dark2"))(nc)
    } else{
        som.cols <- brewer.pal(n = nc, name = "Dark2")
    }
    
    
    #scatterplot with ellipses (PCA with SOM clustering)
    j <- ggplot() +
        geom_point(
            data = pca_score[, 1:2],
            aes(
                PC1,
                PC2,
                col = as.factor(cluster.labs),
                pch = symbol,
                text = paste(
                    text,
                    "<br>SOM Cluster: ",
                    cluster.labs,
                    "<br> PC1: ",
                    PC1,
                    "<br> PC2: ",
                    PC2
                ),
                # tooltip = text,
                # data_id = text
            ),
            size = 2
        ) +
        stat_ellipse(data = pca_score[, 1:2], aes(
            x = PC1,
            y = PC2,
            color = as.factor(cluster.labs),
            text = paste("SOM Cluster: ", cluster.labs)
        )) +
        labs(y = pca_labs[2], x = pca_labs[1], title = "PCA with SOM clustering") +
        scale_color_manual(values = som.cols, name = "SOM cluster")
    if (length(symbol) > 1) {
        j <- j +
            scale_shape_manual(
                values = rep(
                    ggsymbol_list[1:13],
                    length(unique(symbol)),
                    length.out = length(unique(symbol))
                ),
                name = input$rowPoint
            )
    }
    
    
    if (length(unique(symbol)) > 13) {
        showNotification(
            "PCA with SOM clustering plot only takes 6 symbols. Your data has more than 6 categories. The point symbols will repeat."
        )
    }
    
    
    piechart <- as.data.frame(som.counts)
    pie <-
        plot_ly(
            piechart,
            type = 'pie',
            labels = ~ cluster.labs,
            values = ~ Freq,
            textposition = 'inside',
            textinfo = 'value+percent',
            hoverinfo = 'text',
            text = ~ paste('Cluster: ', cluster.labs, '<br> ', Freq, " points"),
            marker = list(colors = som.cols)
        ) %>%
        layout(title = "Number of points in each SOM cluster")
    if (length(color) > 1) {
        stat <- as.data.frame(table(cluster = cluster.labs, color = color))
    } else{
        stat <-
            as.data.frame(table(cluster = cluster.labs, color = rep("All", length(
                cluster.labs
            ))))
        
    }
    suppressWarnings(
        heatmap <- plot_ly(
            x = stat[, 2],
            y = stat[, 1],
            z = stat[, 3],
            type = "heatmap"
        ) %>%
            colorbar(title = "Number of points") %>%
            add_annotations(
                text = stat[, 3],
                showarrow = FALSE,
                font = list(
                    color = '#FFFFFF',
                    family = 'sans serif',
                    size = 18
                )
            ) %>%
            layout(
                title = "Loading score Heatmap",
                xaxis = list(title = input$rowColor),
                yaxis = list(title = "SOM Cluster")
            )
    )
    
    map <- girafe(
        ggobj = point_dist,
        options = list(
            opts_sizing(rescale = T, width = 1),
            opts_hover_inv(css = "opacity:0.1;"),
            opts_hover(css = "fill:grey;stroke:black"),
            opts_zoom(max = 5)
        )
    ) #hover a click options not yet available due to clipping problem
    pies <- girafe(ggobj = pies,
                   options = list(opts_sizing(
                       rescale = TRUE, width = 1
                   ),
                   opts_zoom(max = 5)))
    
    pca_som <- ggplotly(j, tooltip = c("text"))
    
    som <- list(
        map = map,
        pies = pies,
        counts = pie,
        pca_som = pca_som,
        heatmap_som = heatmap
    )
    
    return(som)
})

output$SOM_map <- renderGirafe({
    SOM_graph()$map
})

output$SOM_counts <- renderPlotly({
    SOM_graph()$counts
    
    
})
output$SOM_pies <- renderGirafe({
    SOM_graph()$pies
    
    
})
output$SOM_pca <- renderPlotly({
    SOM_graph()$pca_som
    
    
})
output$SOM_heatmap <- renderPlotly({
    SOM_graph()$heatmap_som
    
    
})
