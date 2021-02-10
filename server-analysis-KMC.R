KMC_analysis <- reactive({
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    req(input$ncluster)
    ncluster <- input$ncluster
    withProgress(
        value = 0,
        message = paste("Performing K-means clustering with K =", ncluster),
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
            
            res.km <-
                eclust(
                    mat,
                    "kmeans",
                    k = input$ncluster,
                    graph = F,
                    verbose = F
                )
            
            setProgress(value = 0.5, detail = "preparing data ...")
            return(res.km)
            setProgress(value = 1, detail = "done!")
            
        }
    )
})



KMC_plotly <- reactive({
    need(!is.null(KMC_analysis()),
         message = "K-means not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    req(input$ncluster)
    ncluster <- input$ncluster
    res.km <- KMC_analysis()
    inputData <- inputDataAnalysis()
    symbol_list <- params()$symbol_list
    color_list <- params()$color_list
    # mapped_colors<-params()$mapped_colors
    # mapped_symbols<-params()$mapped_symbols
    symbol <- params()$symbol_column
    color <- params()$color_column
    name <- params()$name
    text <- params()$text
    withProgress(value = 0,
                 message = "Making graph...",
                 detail = "...",
                 {
                     clst <- paste0("C", res.km$cluster)
                     
                     if (input$rowColor != "") {
                         stable <-
                             as.data.frame(table(inputData$data[, input$rowColor], clst))
                         
                         cluster_counts <-
                             plot_ly(
                                 x = stable[, 2],
                                 y = stable[, 1],
                                 z = stable[, 3],
                                 type = "heatmap"
                             ) %>%
                             colorbar(title = "Number of points",
                                      len = 0.2,
                                      y = 0.2) %>%
                             add_annotations(
                                 text = stable[, 3],
                                 showarrow = FALSE,
                                 font = list(
                                     color = '#FFFFFF',
                                     family = 'sans serif',
                                     size = 18
                                 )
                             ) %>%
                             layout(
                                 #title = "Category vs Clusters Heatmap",
                                 xaxis = list(title = input$rowColor),
                                 yaxis = list(title = "Cluster"),
                                 annotations = list(
                                     list(
                                         x = 0 ,
                                         y = 0.92,
                                         text = "Category vs Clusters Heatmap",
                                         showarrow = F,
                                         xref = 'paper',
                                         yref = 'paper'
                                     )
                                 )
                             )
                        
                     }
                     
                     setProgress(value = 0.5, detail = "preparing graph ...")
                     kmc_scatterplot <-
                         fviz_cluster(
                             res.km,
                             geom = 'point',
                             main = "K-means clustering",
                             pointsize = 2,
                             ggtheme = theme_minimal(),
                             shape = 21,
                             palette = "Set1",
                             text = text
                         )
                     kmc_scatterplot <- ggplotly(kmc_scatterplot)
                     
                     kmc_scatterplot <-
                         subplot(
                             kmc_scatterplot,
                             cluster_counts,
                             nrows = 2,
                             shareX = FALSE,
                             titleX = TRUE,
                             shareY = FALSE,
                             titleY = TRUE,
                             heights = c(0.8, 0.2),
                             margin = 0.07,
                             which_layout = 1
                         )
                     
                     
                     return(kmc_scatterplot)
                     setProgress(value = 1, detail = "done!")
                     
                 })
    
})


KMC_optimal <- reactive({
    need(!is.null(KMC_analysis()),
         message = "K-means not calculated. Check that there are no missing entries in the data. Check that there is at least one numeric variable in the data.")
    
    withProgress(value = 0,
                 message = "Calculating optimal k.",
                 detail = "This can take several minutes. Please wait...",
                 {
                     req(input$optimal_method, inputDataAnalysis())
                     inputData <- inputDataAnalysis()
                     method <- input$optimal_method
                     mat <- inputData$mat
                     if (method != "") {
                         if (method == "Elbow method") {
                             set.seed(123)
                             
                             m <-
                                 fviz_nbclust(mat, kmeans, method = "wss") +
                                 geom_vline(xintercept = 4, linetype = 2) +
                                 labs(subtitle = "Elbow method")
                         } else if (method == "Silhouette method") {
                             set.seed(123)
                             
                             m <-
                                 fviz_nbclust(mat, kmeans, method = "silhouette") +
                                 labs(subtitle = "Silhouette method")
                         } else if (method == "GAP statistic method") {
                             set.seed(123)
                             
                             m <-
                                 fviz_nbclust(mat, kmeans, method = "gap_stat") +
                                 labs(subtitle = "Gap statistic method")
                         }
                     }
                     
                     setProgress(value = 0.5, detail = "plotting ...")
                     
                     return(ggplotly(m))
                     setProgress(value = 1, detail = "done!")
                     
                 })
    
})

output$kmc_scatterplot <- renderPlotly({
    KMC_plotly()
})

output$optimal_clusters <- renderPlotly({
    KMC_optimal()
})
