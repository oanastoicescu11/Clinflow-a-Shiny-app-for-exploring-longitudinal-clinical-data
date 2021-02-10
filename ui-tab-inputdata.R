
#UI file for the "Upload and Preprocess navbar with all the tabs
tabPanel("Upload and preprocess",
         #uploading csv file 
         # App title ----
         titlePanel("Input Data"),
         
         # Sidebar layout with input and output definitions ----
         sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
                 checkboxInput("own_data", "Upload your own csv file", value = FALSE),
                 # Input: Select separator ----
                 uiOutput("upload_options"),
                 width = 1),
             
             # Main panel for displaying outputs ----
             mainPanel(
                 tabsetPanel(
                     tabPanel("Original Data Summary",
                              # Output: Data file ----
                              uiOutput("choose_id"),
                              column(width = 12,
                                     DT::dataTableOutput(outputId = "table_input"),
                                     style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                              box(title = "View summary and structure of the original dataset",
                                  width = 12,
                                  status = "warning", 
                                  solidHeader = FALSE, 
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  div(
                                      style = 'overflow-x: scroll',
                                      # uiOutput(outputId = "show_filtered_structure"),
                                      # actionButton("view_original_summary", "View data summary"),
                                      # actionButton("view_original_structure", "View data structure"),
                                      verbatimTextOutput(outputId ="summary_input"),
                                      verbatimTextOutput(outputId ="structure_input")
                                  )
                              )),
                     tabPanel("Preprocessed Data
                              ",
                              # Output: Data file ----
                              div(column(width = 12,
                                         hr(),
                                         h2("Visit Data"),
                                         DT::dataTableOutput(outputId = "table_visits"),
                                         style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                  
                                  downloadButton("downloadVisitData", "Download visit data as .csv")),
                              box(title = "View summary and structure of the Visit dataset",
                                  width = 12,
                                  status = "warning", 
                                  solidHeader = FALSE, 
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  div(
                                      style = 'overflow-x: scroll',
                                      verbatimTextOutput(outputId ="summary_visit"),
                                      verbatimTextOutput(outputId ="structure_visit")
                                  )
                              ),
                              # actionButton("view_visit_summary", "View visit summary"),
                              # actionButton("view_visit_structure", "View visit structure"),
                              # 
                              div(column(width = 12,
                                         hr(),
                                         h2("Patient Data"),
                                         DT::dataTableOutput(outputId = "table_constants"),
                                         style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                  
                                  downloadButton("downloadConstantData", "Download patient data as .csv")),
                              box(title = "View summary and structure of the Patient dataset",
                                  width = 12,
                                  status = "warning", 
                                  solidHeader = FALSE, 
                                  collapsible = TRUE,
                                  collapsed = TRUE,
                                  div(
                                      style = 'overflow-x: scroll',
                                      verbatimTextOutput(outputId = "summary_constant"),
                                      verbatimTextOutput(outputId = "structure_constant")
                                      
                                  )
                              )
                              # actionButton("view_constant_summary", "View patient summary"),
                              # 
                              # actionButton("view_constant_structure", "View patient structure"),
                              
                     ),
                     tabPanel("Filter Data",
                              # Output: Data file ----
                              fluidRow(
                                  
                                  div(#style="display: inline-block",
                                      uiOutput(outputId = "dataset_choice"),
                                      column(
                                          width = 3,
                                          uiOutput(outputId = "variable2_choice"),
                                          helpText("Subset a group to apply filter on.
                                          Entries that are not part of the group will remain unchanged in the dataset. 
                                        If nothing is selected, the filter will be applied to the whole dataset."),
                                          helpText("Tip: for more precision in the the numeric sliders, click on the slider end and then
                                                   use arrows on the keyboard to move the slider to the exact value."),
                                          filterDF_UI("grouping"),
                                      ),
                                      column(
                                          width = 3,
                                          uiOutput(outputId = "variable_choice"),
                                          helpText("Filter Options are updated to match the selected group.
                                                    Subset your data then click <Filter Table> to view your subset.
                                                   Click <Update Table> to save the filtered table. 
                                                   Click <Reset Table> to get back the original table."),
                                          uiOutput(outputId = "filter_button"),
                                          uiOutput(outputId = "filter_update"),
                                          uiOutput(outputId = "filter_reset"),
                                          filterDF_UI("filtering"),
                                      ),
                                      
                                      column(
                                          width = 6,
                                          box(title = "View summary of the filtered dataset",
                                              width = 14,
                                              status = "warning", 
                                              solidHeader = FALSE, 
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              div(
                                                  style = 'overflow-x: scroll',
                                                  # uiOutput(outputId = "show_filtered_structure"),
                                                  # uiOutput(outputId = "show_filtered_summary"),
                                                  verbatimTextOutput(outputId = "res_summary"),
                                                  #verbatimTextOutput(outputId = "res_str")
                                                  
                                              )),
                                          hr(),
                                          box(title = "View Tables",
                                              width = 14,
                                              status = "warning", 
                                              solidHeader = FALSE, 
                                              collapsible = TRUE,
                                              progressBar(
                                                  id = "pbar", value = 100,
                                                  total = 100, display_pct = TRUE
                                              ),
                                              
                                              div(
                                                  style = 'overflow-x: scroll',
                                                  hr(),
                                                  h2("Filtered Table"),
                                                  DT::dataTableOutput(outputId = "table"),
                                              ),
                                              hr(),
                                              
                                              div(
                                                  style = 'overflow-x: scroll',
                                                  h2("Updated Patient Data"),
                                                  DT::dataTableOutput(outputId = "test_reactivedata"),
                                              ),
                                              downloadButton("downloadFilteredConstantData", "Download filtered patient data as .csv"),
                                              hr(),
                                              div(
                                                  style = 'overflow-x: scroll',
                                                  h2("Updated Visit Data"),
                                                  DT::dataTableOutput(outputId = "test_visitdata"),
                                              ),
                                              downloadButton("downloadFilteredVisitData", "Download filtered visit data as .csv")
                                          ),
                                          
                                      )))
                     ),
                     
                     tabPanel("Missing Data",
                              # Output: Data file ----
                              wellPanel(
                                  fluidRow(
                                      column(
                                          width = 2,                                 
                                          
                                          selectizeInput(
                                              "missing_data",
                                              "Select a dataset to display missing value map",
                                              choices = c("Patient data", "Visit data"),
                                              multiple = F,
                                              options = list(placeholder = 'Please select one dataset'),
                                              selected = ""
                                          ),
                                          selectizeInput("missplot", "Type of chart:", choices = c("Missing data map", "Scatter plot of missing mechanism"),
                                                         multiple = F,
                                                         options = list(placeholder = 'Choose a plot'),
                                                         selected = "Missing data map"),
                                          
                                          conditionalPanel(
                                              condition = "input.missplot == 'Scatter plot of missing mechanism'",
                                              uiOutput('misschartOptions'),
                                              actionButton("delete_selected", "Delete selected entries")
                                          ),
                                          
                                      ),
                                      div(
                                          column(width = 10,
                                                 conditionalPanel(
                                                     condition = "input.missplot == 'Missing data map'",
                                                     plotOutput("missing_map")
                                                 ),
                                                 conditionalPanel(
                                                     condition = "input.missplot == 'Scatter plot of missing mechanism'",
                                                     
                                                     
                                                     h3("Scatter plot"),
                                                     plotlyOutput("miss_scatterplot"),
                                                     verbatimTextOutput("miss_click"),
                                                     column(width = 5,
                                                            h2("Data Table"),
                                                            div(
                                                                DT::dataTableOutput(outputId = "miss_table_keep"),
                                                                style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
                                                     
                                                     column(width = 5,
                                                            h2("Selected Data"),
                                                            div(
                                                                DT::dataTableOutput(outputId = "miss_table_selected"),
                                                                style = "height:auto; overflow-y: scroll;overflow-x: scroll;"))
                                                     
                                                     
                                                 ))),
                                      
                                      
                                  )
                                  
                                  
                              )
                     ),
                     tabPanel("Create new patient feature",
                              # Output: Data file ----
                              fluidRow(
                                  column(
                                      width = 3,
                                      textInput(
                                          "newvar_name",
                                          "Type a name for the new variable",
                                          value = ""
                                          
                                      ),
                                      uiOutput(outputId = "range_var"),
                                      uiOutput(outputId = "newvar_params"),
                                      uiOutput(outputId = "func_to_apply"),
                                      actionButton("update", "Update Table"),
                                      uiOutput(outputId ="save_newvar_btn"),
                                      span(textOutput(outputId = "norows"),style="color:red"),
                                      span(textOutput(outputId = "nothing_to_save"),style="color:red"),
                                      span(textOutput(outputId = "choose_different_name"),style="color:red"),
                                      span(textOutput(outputId = "type_name"),style="color:red")),
                                  
                                  column(width = 9,
                                         hr(),
                                         box(title = "View Tables",
                                             width = 16,
                                             status = "warning", 
                                             solidHeader = FALSE, 
                                             collapsible = TRUE,
                                             collapsed = FALSE,
                                             h2("Updated table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "view"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                             hr(),
                                             h2("Saved table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "check"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                             downloadButton("downloadNewConstanttData", "Download new patient data as .csv")),
                                         
                                         div(
                                             hr(),
                                             box(title = "View Summary",
                                                 width = 16,
                                                 status = "warning", 
                                                 solidHeader = FALSE, 
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "new_summary")
                                             ),
                                             box(title = "View Structure",
                                                 width = 16,
                                                 status = "warning", 
                                                 solidHeader = FALSE, 
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "structure")
                                             )
                                         )
                                         
                                  ))
                              
                     ),
                     tabPanel("Create new categorical feature",
                              # Output: Data file ----
                              fluidRow(
                                  column(
                                      width = 3,
                                      textInput(
                                          "newcat_name",
                                          "Type a name for the new categorical variable",
                                          value = ""
                                          
                                      ),
                                      uiOutput(outputId = "dataset_categorize"),
                                      uiOutput(outputId = "variable_choice_categorize"),
                                      uiOutput(outputId = "categorize_intervals"),
                                      actionButton("update_categorize", "Update Table"),
                                      actionButton("save_newvar_categorize_btn", "Save Table"),
                                      span(textOutput(outputId = "norows_cat"),style="color:red"),
                                      span(textOutput(outputId = "invalid_vect"),style="color:red"),
                                      span(textOutput(outputId = "nothing_to_cat_save"),style="color:red"),
                                      span(textOutput(outputId = "type_cat_name"),style="color:red"),
                                      span(textOutput(outputId = "same_name"),style="color:red"),
                                      span(textOutput(outputId = "choose_different_cat_name"),style="color:red")
                                  ),
                                  
                                  column(width = 9,
                                         hr(),
                                         box(title = "View Tables",
                                             width = 16,
                                             status = "warning", 
                                             solidHeader = FALSE, 
                                             collapsible = TRUE,
                                             collapsed = FALSE,
                                             hr(),
                                             h2("Updated table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "view_categorize"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                             hr(),
                                             h2("Saved table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "check_categorize"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
                                         
                                         hr(),
                                         div(
                                             box(title = "View Summary",
                                                 width = 16,
                                                 status = "warning", 
                                                 solidHeader = FALSE, 
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "summary_categorize")
                                             ),
                                             box(title = "View Structure",
                                                 width = 16,
                                                 status = "warning", 
                                                 solidHeader = FALSE, 
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "structure_categorize")
                                             )
                                             
                                         ))
                              )
                              
                     ),
                     tabPanel("Create new numeric feature",
                              # Output: Data file ----
                              fluidRow(
                                  column(
                                      width = 3,
                                      textInput(
                                          "new_num_name",
                                          "Type a name for the new numeric variable",
                                          value = ""
                                          
                                      ),
                                      uiOutput(outputId = "dataset_numerize"),
                                      uiOutput(outputId = "variable_choice_numerize"),
                                      uiOutput(outputId = "numerize_order"),
                                      actionButton("update_numerize", "Update Table"),
                                      actionButton("save_newvar_numerize_btn", "Save Table"),
                                      span(textOutput(outputId = "norows_num"),style="color:red"),
                                      span(textOutput(outputId = "nothing_to_num_save"),style="color:red"),
                                      span(textOutput(outputId = "type_num_name"),style="color:red"),
                                      span(textOutput(outputId = "same_new_name"),style="color:red"),
                                      span(textOutput(outputId = "choose_different_num_name"),style="color:red")
                                  ),
                                  
                                  column(width = 9,
                                         hr(),
                                         box(title = "View Tables",
                                             width = 16,
                                             status = "warning",
                                             solidHeader = FALSE,
                                             collapsible = TRUE,
                                             collapsed = FALSE,
                                             hr(),
                                             h2("Updated table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "view_numerize"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;"),
                                             hr(),
                                             h2("Saved table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "check_numerize"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
                                         
                                         hr(),
                                         div(
                                             box(title = "View Summary",
                                                 width = 16,
                                                 status = "warning",
                                                 solidHeader = FALSE,
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "summary_numerize")
                                             ),
                                             box(title = "View Structure",
                                                 width = 16,
                                                 status = "warning",
                                                 solidHeader = FALSE,
                                                 collapsible = TRUE,
                                                 collapsed = TRUE,
                                                 verbatimTextOutput(outputId = "structure_numerize")
                                             )
                                             
                                         ))
                              )
                              
                     ),
                     tabPanel("Outliers",
                              fluidRow(
                                  div(
                                      column(
                                          width = 2,
                                          
                                          selectizeInput(
                                              "outliers_data",
                                              "Select a dataset to display outliers plot",
                                              choices = c("Patient data", "Visit data"),
                                              multiple = F,
                                              options = list(placeholder = 'Please select one dataset'),
                                              selected = ""
                                          ),
                                          uiOutput('chartOptions'),
                                          actionButton("delete_outliers", "Delete Outliers")
                                      ),
                                      column(width = 5,
                                             h3("Scatter plot"),
                                             plotlyOutput("scatterplot"),
                                             verbatimTextOutput("click")
                                      ),
                                      column(width = 5,
                                             h3("Box plot"),
                                             plotlyOutput("boxplot")
                                             
                                      )),
                                  
                                  
                              ),
                              fluidRow(
                                  div(
                                      helpText("To identify points: Click on or select point(s). To select multiple points use the Select option provided in the plot"),
                                      helpText("To delete the selected outliers: Click on <Delete Outliers>. The points in the Outlier Table will be removed from the Data"),
                                      helpText("Rows that contain missing values in the plotted variables are not shown in the plot!"),
                                      helpText("Tip: You can get the deleted points back by navigating to the <Filter Data> tab and clicking <Reset Table>"),
                                      
                                  ),
                                  hr(),
                                  div(
                                      column(width = 6,
                                             h2("Data Table"),
                                             div(
                                                 DT::dataTableOutput(outputId = "table_keep"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;")),
                                      
                                      column(width = 6,
                                             h2("Outlier Data"),
                                             div(
                                                 DT::dataTableOutput(outputId = "table_outliers"),
                                                 style = "height:auto; overflow-y: scroll;overflow-x: scroll;"))
                                  ),
                                  
                                  div(
                                      column(width = 12,
                                             hr(),
                                             h4("X Axis Variable Summary"),
                                             verbatimTextOutput("xAxisSummary"),
                                             h4("Y Axis Variable Summary"),
                                             verbatimTextOutput("yAxisSummary"))
                                  ))
                              
                              
                     )
                 )
             )
         )
)

