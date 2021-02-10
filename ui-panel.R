tabPanel(
    "Panel data",
    titlePanel("Create panel data"),
    sidebarLayout(sidebarPanel(
        div(
            uiOutput("choose_timestamp_var"),
            uiOutput("aggregate"),
            uiOutput("create_timestamp"),
            uiOutput("choose_panel_max_vars"),
            uiOutput("choose_panel_sum_vars"),
            uiOutput("choose_panel_mean_vars"),
            uiOutput("choose_panel_categorical_vars"),
            uiOutput("nonlogical_levels"),
            uiOutput("choose_panel_constant_vars"),
            uiOutput("checkbox_mother_panel"),
            span(textOutput(outputId = "noage"), style = "color:red"),
            span(textOutput(outputId = "invalid_panel_timestamp"), style =
                     "color:red"),
            span(textOutput(outputId = "enter_time_points"), style =
                     "color:red"),
            actionButton(
                "create_panel",
                "Create Panel Data",
                icon = icon("arrow-circle-right"),
                width = 200
            )
            
        ),
        
        width = 2
    ),
    mainPanel(tabsetPanel(
        tabPanel(
            "Panel Data",
            column(
                width = 12,
                DT::dataTableOutput(outputId = "test_panel"),
                style = "height:auto; overflow-y: scroll;overflow-x: scroll;"
            ),
            downloadButton("downloadPanelData", "Download panel data as .csv")
        ),
        tabPanel(
            "Charts for Panel Data",
            column(
                width = 2,
                selectizeInput(
                    "panel_charts",
                    "Type of chart:",
                    choices = c(
                        "Time trend plot",
                        "Multi-layered animated plot",
                        "Faceted plot",
                        "Time-trend boxplot"
                    ),
                    multiple = F,
                    options = list(placeholder = 'Choose a plot'),
                    selected = "Time trend plot"
                ),
                uiOutput("panel_options"),
                uiOutput("display_table")
            ),
            column(
                width = 10,
                wellPanel(
                    style = "width:1200px;height:1000px",
                    conditionalPanel(condition = "input.panel_charts == 'Time trend plot'",
                                     plotlyOutput("time_trend")),
                    
                    conditionalPanel(condition = "input.panel_charts == 'Multi-layered animated plot'",
                                     plotlyOutput("layeredplot")),
                    conditionalPanel(condition = "input.panel_charts == 'Faceted plot'",
                                     plotlyOutput("facetedplot")),
                    conditionalPanel(condition = "input.panel_charts == 'Time-trend boxplot'",
                                     plotlyOutput("boxtrend")),
                    helpText(
                        "Click on the plot to display visit entries for individual patients in the table below"
                    ),
                    div(DT::dataTableOutput(outputId = "table_clicked"), style = "height:auto; overflow-y: scroll;overflow-x: scroll;")
                )
            ),
            
            helpText("Hover with the cursor on the points to see the data entries."),
            helpText(
                "To zoom: click and drag over an area in the plot, or use the zooming icon"
            )
            
            
        )
    )))
    
)