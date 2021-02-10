#visualize a missing data map

miss_datasetInput <- reactive({
  #create reactive dataset according to choice
  if (input$missing_data == "Patient data") {
    df <- datasets$constant_data
  } else if (input$missing_data == "Visit data") {
    df <- datasets$visit_data
  }
  return(df)
})
output$misschartOptions <- renderUI({
  #choose variables to plot
  if (is.null(miss_datasetInput())) {
  }
  else {
    list(
      selectizeInput(
        "missxAxisSelector",
        "X Axis Variable",
        colnames(miss_datasetInput())
      ),
      selectizeInput(
        "missyAxisSelector",
        "Y Axis Variable",
        colnames(miss_datasetInput())
      )
    )
  }
})

miss_vals <- reactiveValues(#define reactive values for:
  data = NULL,
  data_keep = NULL,
  data_exclude = NULL)

observe({
  #original data
  miss_vals$data <- miss_datasetInput()
  miss_vals$data_keep <- miss_datasetInput()
  
})


## Original Datatable
output$miss_table_keep <- renderDT({
  miss_vals$data_keep
}, options = list(pageLength = 5))

#Datatable of selected entries

output$miss_table_selected <- renderDT({
  miss_vals$data_exclude
}, options = list(pageLength = 5))



# mechanism for managing selected points
miss_keys <- reactiveVal()


observeEvent(event_data("plotly_click", source = "missing", priority = "event"),
             {
               req(
                 miss_vals$data,
                 input$missxAxisSelector,
                 input$missyAxisSelector,
                 event_data("plotly_click", source = "missing")$key
               )
               is_selected <- NULL
               selected <- NULL
               dat <- NULL
               key_new <- event_data("plotly_click", source = "missing")$key
               key_old <- miss_keys()
               
               if (key_new %in% key_old) {
                 miss_keys(setdiff(key_old, key_new))
               } else {
                 miss_keys(c(key_new, key_old))
               }
               #get indices of selected points
               is_selected <- rownames(miss_vals$data) %in% miss_keys()
               selected <-
                 as.numeric(rownames(miss_vals$data)[which(is_selected)])
               
               #delete the selected points from data_keep and put them in data_exclude
               miss_vals$data_keep <- miss_vals$data[!is_selected,]
               miss_vals$data_exclude <- miss_vals$data[is_selected,]
               
               #make a dataset with only the plotted variables for visual purpose
               dat <-
                 miss_vals$data[, c(input$missxAxisSelector, input$missyAxisSelector)]
               #create a factor variable with missing and non missing values
               dat$missing <- "Not Missing"
               if (any(!complete.cases(dat))) {
                 dat[!complete.cases(dat), ]$missing <- "Missing"
               }
               dat$missing <- as.factor(dat$missing)
               indices <- list()
               rownames(dat) <- c(1:nrow(dat))
               
               #initialize plotly proxy
               p <- plotlyProxy("miss_scatterplot", session)
               for (i in 1:2) {
                 #get the indices for each factor level (because plotly plots different traces for each factor level with indices starting from 0)
                 indices[[i]] <-
                   rownames(dat[which(dat$missing == levels(dat$missing)[i]),])
                 plotlyProxyInvoke(p,
                                   "restyle", #mode = "markers",
                                   # could also do list(marker = list(size = input$marker))
                                   # but that overwrites the existing marker definition
                                   # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                   list(marker.line = list(
                                     #change color of outline of selected points
                                     color = as.vector(ifelse(is_selected[as.numeric(indices[[i]])], 'black', 'grey')),
                                     
                                     width = 2
                                   )), c(i - 1))
                 
               }
               
             })

observeEvent(event_data("plotly_selected", source = "missing", priority = "event"),
             {
               #this does the same thing as the click event, but for the selected event
               
               req(miss_vals$data,
                   event_data("plotly_selected", source = "missing")$key)
               dat <- NULL
               is_selected <- NULL
               key_new <- event_data("plotly_selected", source = "missing")$key
               key_old <- miss_keys()
               #miss_keys(c(key_new, key_old))
               if (any(key_new %in% key_old)) {
                 miss_keys(setdiff(key_old, key_new))
               } else {
                 miss_keys(c(key_new, key_old))
               }
               is_selected <- rownames(miss_vals$data) %in% miss_keys()
               
               miss_vals$data_keep <- miss_vals$data[!is_selected,]
               miss_vals$data_exclude <- miss_vals$data[is_selected,]
               
               dat <-
                 miss_vals$data[, c(input$missxAxisSelector, input$missyAxisSelector)]
               dat$missing <- "Not Missing"
               if (any(!complete.cases(dat))) {
                 dat[!complete.cases(dat), ]$missing <- "Missing"
               }
               dat$missing <- as.factor(dat$missing)
               
               p <- plotlyProxy("miss_scatterplot", session)
               indices <- list()
               
               for (i in 1:2) {
                 indices[[i]] <-
                   rownames(dat[which(dat$missing == levels(dat$missing)[i]),])
                 plotlyProxyInvoke(p,
                                   "restyle", #mode = "markers",
                                   # could also do list(marker = list(size = input$marker))
                                   # but that overwrites the existing marker definition
                                   # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                   list(marker.line = list(
                                     color = as.vector(ifelse(is_selected[as.numeric(indices[[i]])], 'black', 'grey')),
                                     
                                     width = 2
                                   )), c(i - 1))
                 
               }
               
             })

observeEvent(event_data("plotly_doubleclick", source = "missing"), {
  #double click to clear selection
  req(miss_vals$data)
  miss_keys(NULL)
  miss_vals$data_keep <- miss_vals$data
  miss_vals$data_exclude <- NULL
  plotlyProxy("miss_scatterplot", session) %>%
    plotlyProxyInvoke("restyle",
                      #mode = "markers",
                      # could also do list(marker = list(size = input$marker))
                      # but that overwrites the existing marker definition
                      # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                      list(marker.line = list(color = 'grey',
                                              width = 2)))
  
})

observeEvent(event_data("plotly_deselect", source = "missing"), {
  #deselect to clear selection (this is also double click but when <select> mode is enabled in the plot)
  req(miss_vals$data)
  miss_keys(NULL)
  miss_vals$data_keep <- miss_vals$data
  miss_vals$data_exclude <- NULL
  plotlyProxy("miss_scatterplot", session) %>%
    plotlyProxyInvoke("restyle",
                      #mode = "markers",
                      # could also do list(marker = list(size = input$marker))
                      # but that overwrites the existing marker definition
                      # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                      list(marker.line = list(color = 'grey',
                                              width = 2)))
  
})

output$miss_scatterplot <- renderPlotly({
  #render the plot
  req(miss_vals$data,
      input$missxAxisSelector,
      input$missyAxisSelector)
  
  dat <- miss_vals$data
  x <- as.numeric(dat[, input$missxAxisSelector])
  y <- as.numeric(dat[, input$missyAxisSelector])
  
  row <- as.numeric(rownames(miss_vals$data))
  # if(all(is.na(dat[,input$missxAxisSelector]))|all(is.na(dat[,input$missyAxisSelector]))){
  #     showNotification("Warning: One of the variables has 0 non-NA values. Need at least one non-NA value",
  #                      duration = NULL, type = "warning")
  #     missing_scatterplot <- NULL
  # }else{
  missing_scatterplot <- dat %>% ggplot() +
    naniar::geom_miss_point(aes(x = x, y = y, key = row))
  
  
  
  if (is.factor(dat[, input$missxAxisSelector])) {
    missing_scatterplot <- missing_scatterplot +
      scale_x_continuous(breaks = as.integer(x), label = as.character(dat[, input$missxAxisSelector]))
  }
  
  if (is.factor(dat[, input$missyAxisSelector])) {
    missing_scatterplot <- missing_scatterplot +
      scale_y_continuous(breaks = as.integer(y), label = as.character(dat[, input$missyAxisSelector]))
  }
  
  
  
  xax <- list(title = input$missxAxisSelector)
  yax <- list(title = input$missyAxisSelector)
  
  return(
    ggplotly(
      missing_scatterplot,
      source = "missing",
      tooltip = c("x", "y")
    ) %>%
      layout(xaxis = xax, yaxis = yax)
  )
  #}
})


output$miss_click <- renderPrint({
  #in this field clicked points appear for checking (in case the highlighted point is not the clicked one)
  d <- event_data("plotly_click", source = "missing")
  
  if (is.null(d))
    "click events appear here (double-click to clear)"
  else
    d
})


output$missing_map <- renderPlot({
  # map of missing values (non interactive)
  if (!is.null(original_data())) {
    if (input$missing_data == "Patient data") {
      df <- datasets$constant_data
      withProgress(value = 0.5,
                   message = "Rendering Table",
                   detail = "...",
                   {
                     missmap(df,
                             col = c("black", "grey"),
                             legend = TRUE)
                     
                     setProgress(value = 1, detail = "done!")
                   })
    } else if (input$missing_data == "Visit data") {
      df <- datasets$visit_data
      withProgress(value = 0.5,
                   message = "Rendering Table",
                   detail = "...",
                   {
                     missmap(df,
                             col = c("black", "grey"),
                             legend = TRUE)
                     
                     setProgress(value = 1, detail = "done!")
                   })
      
    }
  }
})

observeEvent(input$delete_selected, {
  #update reactive datasets with the data that we keep after deleting outliers
  if (input$missing_data == "Patient data") {
    datasets$constant_data <- miss_vals$data_keep
    datasets$visit_data <-
      datasets$visit_data[datasets$visit_data[, input$choose_id] %in% datasets$constant_data[, input$choose_id],]
    miss_vals$data_exclude <- NULL
  } else if (input$missing_data == "Visit data") {
    datasets$visit_data <- miss_vals$data_keep
    datasets$constant_data <-
      datasets$constant_data[datasets$constant_data[, input$choose_id] %in% datasets$visit_data[, input$choose_id],]
    miss_vals$data_exclude <- NULL
  }
  
})
