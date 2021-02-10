#UI module for preparing the data to be visualized (code adapted from https://github.com/htpmod/HTPmod-shinyApp/tree/master/htpdvis)
library(shiny)
library(shinyBS)

HTML_FOOTER <-
    "This section is refactored from the HTPdvis app by author Dijun Chen (2017). "

tabPanel(
    "Visualize",
    titlePanel("Data visualization tools"),
    sidebarLayout(
        sidebarPanel(
            div(
                id = "div_step2",
                h4(icon("caret-right"), "2. Data Summary"),
                h5(strong("All input data, with size of: ")),
                verbatimTextOutput("uiX1"),
                h5(strong("Numeric data for clustering, with size of: ")),
                verbatimTextOutput("uiX2")
            ),
            div(
                id = "div_step3",
                h4(icon("caret-right"), "3. Customization"),
                uiOutput('uicpRow'),
                helpText(
                    "Values from the dataset corresponding to the points in the plot will be seen on hover."
                ),
                h5(strong("Data preprocessing:")),
                fluidRow(column(
                    6,
                    checkboxInput("center",
                                  strong("Center"),
                                  TRUE)
                ),
                column(6,
                       checkboxInput(
                           "scale",
                           strong("Scale"),
                           TRUE
                       )))
                
            ),
            
            
            width = 2
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(strong(icon("database"), "My Data"),
                         fluidRow(
                             column(
                                 2,
                                 div(
                                     selectizeInput(
                                         "visualization_data",
                                         "Choose Data",
                                         choices = c("patient data", "visit data"),
                                         multiple = F,
                                         options = list(placeholder = 'Please select one dataset'),
                                         selected = ""
                                     ),
                                     id = "div_exampleInUse"
                                 ),
                                 div(
                                     uiOutput("choose_vis_vars"),
                                     uiOutput("choose_cluster_vars")
                                 ),
                                 h5(strong("Treat missing data")),
                                 selectizeInput(
                                     "treatment",
                                     "Choose treatment for missing data",
                                     choices = c("bayesian imputation", "delete rows with NAs", "no treatment"),
                                     multiple = F,
                                     selected = "no treatment"
                                 ),
                                 verbatimTextOutput("test"),
                                 verbatimTextOutput("pltd"),
                                 
                                 actionButton(
                                     "go",
                                     "Go",
                                     icon = icon("arrow-circle-right"),
                                     width = 200
                                 )
                             ),
                             
                             column(
                                 10,
                                 br(),
                                 h2("Input table"),
                                 DT::dataTableOutput("inputData"),
                                 h2("Numeric table"),
                                 DT::dataTableOutput("bayesian"),
                                 br(),
                                 downloadButton("downloadInputData", label = "Save as Table .txt")
                             )
                         )),
                tabPanel(
                    strong(icon("cubes"), "PCA"),
                    br(),
                    fluidRow(
                        column(3,
                               uiOutput("npcs")),
                        
                        column(
                            3,
                            selectizeInput(
                                "pca_charts",
                                "Type of chart:",
                                choices = c("Pairwise 2D plot", "Explained variance", "Heatmap", "3D plot"),
                                multiple = F,
                                options = list(placeholder = 'Choose a plot'),
                                selected = "3D plot"
                            )
                        ),
                        fluidRow(column(
                            12,
                            wellPanel(
                                conditionalPanel(
                                    condition = "input.pca_charts == 'Pairwise 2D plot'",
                                    plotlyOutput("PCA_pairplot", width = "100%", height = "700px")
                                ),
                                
                                conditionalPanel(condition = "input.pca_charts == 'Explained variance'",
                                                 plotlyOutput("PCA_variance")),
                                conditionalPanel(
                                    condition = "input.pca_charts == 'Heatmap'",
                                    plotlyOutput("PCA_heatmap"),
                                    helpText(
                                        "Large (either positive or negative) loading scores indicate that a variable has a strong effect on that principal component."
                                    )
                                ),
                                conditionalPanel(
                                    condition = "input.pca_charts == '3D plot'",
                                    plotlyOutput("PCA_trdplot", width = "100%", height = "700px")
                                )
                            )
                        )),
                        fluidRow(column(
                            6,
                            downloadButton("downloadPpcaScores", label = "PCA Scores.txt"),
                            downloadButton("downloadPpcaVar", label = "Explained variance.txt"),
                            downloadButton("downloadPpcaLoading", label = "Loadings.txt")
                            
                        )),
                        
                    )
                ),
                tabPanel(
                    strong(icon("snowflake-o"), "t-SNE"),
                    br(),
                    fluidRow(
                        column(3,
                               uiOutput("ndims")),
                        
                        column(
                            3,
                            selectizeInput(
                                "tsne_charts",
                                "Type of chart:",
                                choices = c("Pairwise 2D plot", "3D plot"),
                                multiple = F,
                                options = list(placeholder = 'Choose a plot'),
                                selected = "3D plot"
                            )
                        ),
                        fluidRow(column(
                            12,
                            wellPanel(
                                conditionalPanel(
                                    condition = "input.tsne_charts == 'Pairwise 2D plot'",
                                    plotlyOutput("TSNE_pairplot", width = "100%", height = "700px")
                                ),
                                
                                conditionalPanel(
                                    condition = "input.tsne_charts == '3D plot'",
                                    plotlyOutput("TSNE_tridplot", width = "100%", height = "700px")
                                )
                            )
                        )),
                        fluidRow(column(
                            6,
                            downloadButton("downloadTsneScores", label = "TSNE Scores.txt")
                            
                        )),
                    )
                    
                ),
                tabPanel(
                    strong(icon("diamond"), "MDS"),
                    br(),
                    fluidRow(
                        column(3,
                               uiOutput("nmds")),
                        column(
                            3,
                            selectizeInput(
                                "dist_method",
                                "Distance measure",
                                choices = c(
                                    Euclidean = "euclidean",
                                    Maximum = "maximum",
                                    Manhattan = "manhattan",
                                    Canberra = "canberra",
                                    Minkowski = "minkowski"
                                ),
                                multiple = F,
                                options = list(placeholder = 'Distance'),
                                selected = "euclidean"
                            )
                        ),
                        column(
                            3,
                            selectizeInput(
                                "mds_charts",
                                "Type of chart:",
                                choices = c("Pairwise 2D plot", "3D plot"),
                                multiple = F,
                                options = list(placeholder = 'Choose a plot'),
                                selected = "3D plot"
                            )
                        ),
                        fluidRow(column(
                            12,
                            wellPanel(
                                conditionalPanel(
                                    condition = "input.mds_charts == 'Pairwise 2D plot'",
                                    plotlyOutput("MDS_pairplot", width = "100%", height = "700px")
                                ),
                                
                                conditionalPanel(
                                    condition = "input.mds_charts == '3D plot'",
                                    plotlyOutput("MDS_tridplot", width = "100%", height = "700px")
                                )
                            )
                        )),
                        fluidRow(column(
                            6,
                            downloadButton("downloadMdsScores", label = "MDS Scores.txt")
                            
                        )),
                    )
                ),
                tabPanel(
                    strong(icon("life-ring"), "SOM"),
                    br(),
                    fluidRow(
                        column(8,
                               h5(strong(
                                   "Dimensions of the grid"
                               )),
                               fluidRow(
                                   column(
                                       6,
                                       sliderInput(
                                           "xdim",
                                           "x-dimension",
                                           min = 1,
                                           max = 10,
                                           value = 3,
                                           step = 1
                                       )
                                   ),
                                   column(
                                       6,
                                       sliderInput(
                                           "ydim",
                                           "y-dimension",
                                           min = 1,
                                           max = 10,
                                           value = 3,
                                           step = 1
                                       )
                                   )
                               )),
                        column(
                            4,
                            radioButtons(
                                "topo",
                                label = "Topology of the grid",
                                choices = list("Hexagonal" = "hexagonal",
                                               "Rectangular" = "rectangular"),
                                inline = F
                            )
                        ),
                        column(
                            3,
                            selectizeInput(
                                "som_charts",
                                "Type of chart:",
                                choices = c(
                                    "Kohonen map",
                                    "Pies",
                                    "Counts plot",
                                    "PCA with SOM clustering",
                                    "Heatmap count"
                                ),
                                multiple = F,
                                options = list(placeholder = 'Choose a plot'),
                                selected = "Kohonen map"
                            )
                        )
                    ),
                    fluidRow(column(
                        12,
                        wellPanel(
                            conditionalPanel(condition = "input.som_charts == 'Kohonen map'",
                                             girafeOutput("SOM_map")),
                            #
                            # conditionalPanel(
                            #     condition = "input.som_charts == 'Interactive scatterpoint map'",
                            #     plotlyOutput("SOM_interactivemap",width = "100%", height = "700px")
                            # ),
                            conditionalPanel(
                                condition = "input.som_charts == 'Pies'",
                                girafeOutput("SOM_pies", width = "100%", height = "700px")
                            ),
                            conditionalPanel(
                                condition = "input.som_charts == 'Counts plot'",
                                plotlyOutput("SOM_counts", width = "100%", height = "700px")
                            ),
                            conditionalPanel(
                                condition = "input.som_charts == 'Heatmap count'",
                                plotlyOutput("SOM_heatmap", width = "100%", height = "700px")
                            ),
                            conditionalPanel(
                                condition = "input.som_charts == 'PCA with SOM clustering'",
                                plotlyOutput("SOM_pca", width = "100%", height = "700px")
                            )
                            
                        )
                    ))
                ),
                tabPanel(
                    strong(icon("recycle"), "K-MC"),
                    br(),
                    fluidRow(column(
                        4,
                        sliderInput(
                            "ncluster",
                            "Number of clusters",
                            min = 2,
                            max = 10,
                            value = 3,
                            step = 1
                        )
                    ),
                    column(
                        3,
                        selectizeInput(
                            "kmc_charts",
                            "Type of chart:",
                            choices = c("K-means plot", "Optimal number of clusters"),
                            multiple = F,
                            options = list(placeholder = 'Choose a plot'),
                            selected = "K-means plot"
                        )
                    )),
                    fluidRow(wellPanel(
                        conditionalPanel(
                            condition = "input.kmc_charts == 'K-means plot'",
                            plotlyOutput("kmc_scatterplot", width = "100%", height = "500px")
                        ),
                        conditionalPanel(
                            condition = "input.kmc_charts == 'Optimal number of clusters'",
                            column(
                                3,
                                selectizeInput(
                                    "optimal_method",
                                    "Choose a method to calculate optimal number of clusters:",
                                    choices = c("", "Elbow method", "Silhouette method", "GAP statistic method"),
                                    multiple = F,
                                    options = list(placeholder = 'Choose a method'),
                                    selected = ""
                                )
                            ),
                            column(
                                9,
                                plotlyOutput("optimal_clusters", width = "100%", height = "500px")
                            )
                            
                        )
                    ))
                ),
                tabPanel(
                    strong(icon("object-group"), "Charts"),
                    br(),
                    useShinyjs(),
                    fluidRow(
                        column(
                            3,
                            h5(strong("Data for drawing")),
                            uiOutput('uipxyzm'),
                            selectizeInput(
                                "misct",
                                "Type of plot",
                                choices = list(
                                    'One-Variable Plot: numeric Y' = c(
                                        `Density plot` = '1|ggdensity',
                                        `ECDF plot` = '1|ggecdf',
                                        `Histogram plot` = '1|gghistogram',
                                        `QQ plot` = '1|ggqqplot'
                                    ),
                                    
                                    'One-Variable Plot: categorical X' = c(`Percentage box plot` = '4|percent'),
                                    'Two-Variable Plot: categorical X & numeric Y' = c(
                                        `Bar plot` = '2|ggbarplot',
                                        `Box plot` = '2|ggboxplot',
                                        `Line plot` =
                                            '2|ggline',
                                        ## `Pie chart` = '2|ggpie',
                                        `Stripchart` = '2|ggstripchart',
                                        `Violin Plot` = '2|ggviolin'
                                    ),
                                    'Two-Variable Plot: numeric Y & Z' = c(
                                        `Scatter plot with correlation` = '3|stat_cor',
                                        `Scatter plot with stars` = '3|stat_stars'
                                    ),
                                    'Multi-Variable Plot: numeric and categorical M' = c(`Generalized multivariate pair plot` = 'm|pairs')
                                ),
                                multiple = F,
                                options = list(placeholder = 'Choose a plot'),
                                selected = "m|pairs"
                            ),
                            helpText(
                                "Note: graph will be colored according the color customization set on the left panel"
                            )
                        ),
                        column(9,
                               wellPanel(
                                   h5(strong("Plot")),
                                   div(
                                       class = "thumbnail",
                                       plotlyOutput("plotly_chart", width = "100%", height = "700px")
                                   )
                               ))
                    )
                ),
                id = "mainPanel",
                selected = strong(icon("database"), "My Data")
            ),
            br()
        )
    ),
    shiny::tags$div(class = "container-fluid shiny-code-container well",
                    HTML(HTML_FOOTER), tags$a(href="https://github.com/htpmod/HTPmod-shinyApp.git", "Source here."))
)