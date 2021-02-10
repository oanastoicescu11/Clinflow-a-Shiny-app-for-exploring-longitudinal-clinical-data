
datasetInput <- reactive({
  #create reactive dataset according to choice
  if (input$outliers_data == "Patient data") {
    df <- datasets$constant_data
    
  } else if (input$outliers_data == "Visit data") {
    df <- datasets$visit_data
  }
  nums <- sapply(df, is.numeric)
  integ <-
    apply(df[, nums, drop = F], 2, function(x) {
      #if a column is num but has less than 4 distinct values, turn to ordered factor
      length(unique(x)) <= 12 &&
        sum(table(x) == 1) < length(unique(x))
    })
  integ <- names(integ)[integ]
  #convert to factors columns that have only few distinct numeric values
  df[, integ] <- as.data.frame(sapply(df[, integ] , as.factor))
  
  return(df)
})

output$chartOptions <- renderUI({
  #choose variables to plot
  if (is.null(input$outliers_data)) {
  }
  else {
    list(
      selectizeInput(
        "xAxisSelector",
        "X Axis Variable (?xAxis=)",
        colnames(datasetInput())
      ),
      selectizeInput(
        "yAxisSelector",
        "Y Axis Variable (?yAxis=)",
        colnames(datasetInput())
      ),
      selectizeInput(
        "colorBySelector",
        "Color By (?colorBy=) [scatter plot only]:",
        c("Do not color", names(cbind(
          select_if(datasetInput(), is.factor)
        )))
      )
    )
  }
})




sessionval <- session$ns("")

js <- "function(el,x, data){
    var id = el.getAttribute('id');
    var d3 = Plotly.d3;
    el.on('plotly_afterplot', function(event) {
             var out = [];
              d3.select('#' + id + ' g.legend').selectAll('.traces').each(function(){
                var trace = d3.select(this)[0][0].__data__[0].trace;
                out.push([name=trace.name, index=trace.index]);

              });

        Shiny.setInputValue(data.ns + data.x, out);

  });


}" #js function to update traces by name (not indices)

vals <- reactiveValues(
  #define reactive values for:
  data = NULL,
  data_keep = NULL,
  data_exclude = NULL,
  key = NULL
)

observe({
  #original data
  req(input$xAxisSelector, input$yAxisSelector)
  vals$data <- datasetInput() %>% mutate_if(is.factor,
                                            fct_explicit_na,
                                            na_level = "Missing")
  vals$data_keep <- datasetInput()
  #vals$key <- rownames(vals$data %>% drop_na(all_of(input$xAxisSelector),all_of(input$yAxisSelector)))
  vals$key <-
    rownames(vals$data[which(!is.na(vals$data[, input$xAxisSelector]) &
                               !is.na(vals$data[, input$yAxisSelector])),])
})


regression_model <- function(data, x, y) {
  if (x %in% colnames(data) & nrow(data) != 0) {
    d <- data[, c(x, y)]
    d <- setNames(d, c("x", "y"))
    d <- data.frame(sapply(d, as.numeric))
    # fit a simple linear model
    m <- lm(y ~ x, data  = d)
    
    # generate y predictions over a grid of max(d$x) x values
    dpred <- data.frame(x = seq(
      min(d$x, na.rm = T),
      max(d$x, na.rm = T),
      length.out = max(d$x, na.rm = T)
    ))
    dpred$yhat <- predict(m, newdata = dpred)
    
    return(dpred)
  } else{
    return(NULL)
  }
}

model <- reactive({
  req(vals$data_keep, input$xAxisSelector, input$yAxisSelector)
  regression_model(vals$data_keep, input$xAxisSelector, input$yAxisSelector)
  
})

observeEvent(model(), label = "model", {
  req(input$colorBySelector)
  
  linear_model <- list(
    x = model()$x,
    y = model()$yhat,
    type = "scattergl",
    mode = "lines",
    line = list(color = "red"),
    name = "linear model"
  )
  
  if (is.null(input$TraceMapping)) {
    if (input$colorBySelector != "Do not color") {
      if (is.factor(vals$data[, input$colorBySelector])) {
        indices <- length(levels(vals$data[, input$colorBySelector]))
      } else{
        indices <- 1
      }
    } else{
      indices <- 1
    }
  } else{
    traces <- matrix(input$TraceMapping, ncol = 2, byrow = TRUE)
    indices <- as.integer(traces[traces[, 1] == "linear model", 2])
  }

  if (!is.null(model())) {
    plotlyProxy("scatterplot", session) %>%
      plotlyProxyInvoke("deleteTraces", indices) %>%
      plotlyProxyInvoke("addTraces",
                        linear_model)
    
  }
  
})

## Datatable (populated with first five rows unless a graph has been clicked or hovered over)
output$table_keep <- renderDT({
  vals$data_keep
}, options = list(pageLength = 5))

output$table_outliers <- renderDT({
  vals$data_exclude
}, options = list(pageLength = 5))

# mechanism for managing selected points
keys <- reactiveVal()

observeEvent(event_data("plotly_click", source = "outliers", priority = "event"),
             {
               req(vals$data,
                   event_data("plotly_click", source = "outliers")$key)
               is_outlier <- NULL
               outliers <- NULL
               dat <- NULL
               key_new <- event_data("plotly_click", source = "outliers")$key
               key_old <- keys()
               if (length(key_new) <= 1) {
                 #this is so that only points can be clicked not boxes on the box plot
                 if (key_new %in% key_old) {
                   keys(setdiff(key_old, key_new))
                 } else {
                   keys(c(key_new, key_old))
                 }
                 is_outlier <- vals$key %in% keys()
                 outliers <- as.numeric(vals$key[which(is_outlier)])
                 
                 
                 vals$data_keep <- vals$data[-outliers,]
                 vals$data_exclude <- vals$data[outliers,]
                 indices <- list()
                 p <- plotlyProxy("scatterplot", session)
                 
                 marker_line <- list(marker.line = list(color = as.vector(ifelse(
                   is_outlier, "black", "grey"
                 )),
                 width = 2))
                 
                 dat <- vals$data[vals$key,]
                 rownames(dat) <- c(1:nrow(dat))
                 
                 if (input$colorBySelector != "Do not color") {
                   if (is.factor(vals$data[, input$colorBySelector])) {
                     dat[, input$colorBySelector] <-
                       factor(dat[, input$colorBySelector], ordered = FALSE)
                     for (i in 1:length(levels(vals$data[, input$colorBySelector]))) {
                       indices[[i]] <-
                         rownames(dat[which(dat[, input$colorBySelector] == levels(dat[, input$colorBySelector])[i]),])
                       
                       plotlyProxyInvoke(p,
                                         "restyle", #mode = "markers",
                                         # could also do list(marker = list(size = input$marker))
                                         # but that overwrites the existing marker definition
                                         # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                         list(marker.line = list(
                                           color = as.vector(ifelse(is_outlier[as.numeric(indices[[i]])], 'black', 'grey')),
                                           width = 2
                                           
                                         )), c(i - 1))
                       
                     }
                   } else{
                     p %>%
                       plotlyProxyInvoke("restyle", #mode = "markers+lines",
                                         # could also do list(marker = list(size = input$marker))
                                         # but that overwrites the existing marker definition
                                         # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                         marker_line)
                   }
                   
                 } else{
                   p %>%
                     plotlyProxyInvoke("restyle", #mode = "markers",
                                       # could also do list(marker = list(size = input$marker))
                                       # but that overwrites the existing marker definition
                                       # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                       marker_line)
                   
                 }
               }
               
               
             })

observeEvent(event_data("plotly_selected", source = "outliers", priority = "event"),
             {
               req(vals$data,
                   event_data("plotly_selected", source = "outliers")$key)
               
               is_outlier <- NULL
               outliers <- NULL
               dat <- NULL
               
               key_new <- event_data("plotly_selected", source = "outliers")$key
               key_old <- keys()
               #keys(c(key_new, key_old))
               if (any(key_new %in% key_old)) {
                 keys(setdiff(key_old, key_new))
               } else {
                 keys(c(key_new, key_old))
               }
               is_outlier <- vals$key %in% keys()
               outliers <- as.numeric(vals$key[which(is_outlier)])
               
               
               vals$data_keep <- vals$data[-outliers,]
               vals$data_exclude <- vals$data[outliers,]
               indices <- list()
               p <- plotlyProxy("scatterplot", session)
               
               marker_line <- list(marker.line = list(color = as.vector(ifelse(
                 is_outlier, "black", "grey"
               )),
               width = 2))
               
               dat <- vals$data[vals$key,]
               rownames(dat) <- c(1:nrow(dat))
               
               if (input$colorBySelector != "Do not color") {
                 if (is.factor(vals$data[, input$colorBySelector])) {
                   dat[, input$colorBySelector] <-
                     factor(dat[, input$colorBySelector], ordered = FALSE)
                   for (i in 1:length(levels(vals$data[, input$colorBySelector]))) {
                     indices[[i]] <-
                       rownames(dat[which(dat[, input$colorBySelector] == levels(dat[, input$colorBySelector])[i]),])
                     
                     plotlyProxyInvoke(p,
                                       "restyle", #mode = "markers",
                                       # could also do list(marker = list(size = input$marker))
                                       # but that overwrites the existing marker definition
                                       # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                       list(marker.line = list(
                                         color = as.vector(ifelse(is_outlier[as.numeric(indices[[i]])], 'black', 'grey')),
                                         
                                         width = 2
                                         
                                       )), c(i - 1))
                     
                   }
                 } else{
                   p %>%
                     plotlyProxyInvoke("restyle", #mode = "markers",
                                       # could also do list(marker = list(size = input$marker))
                                       # but that overwrites the existing marker definition
                                       # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                       marker_line)
                 }
               } else{
                 p %>%
                   plotlyProxyInvoke("restyle", #mode = "markers",
                                     # could also do list(marker = list(size = input$marker))
                                     # but that overwrites the existing marker definition
                                     # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                                     marker_line)
               }
               
               
               
             })

observeEvent(event_data("plotly_doubleclick", source = "outliers"), {
  req(vals$data)
  keys(NULL)
  vals$data_keep <- vals$data
  vals$data_exclude <- NULL
  plotlyProxy("scatterplot", session) %>%
    plotlyProxyInvoke("restyle",
                      #mode = "markers",
                      # could also do list(marker = list(size = input$marker))
                      # but that overwrites the existing marker definition
                      # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                      list(marker.line = list(color = 'grey',
                                              width = 2)))
  
})

observeEvent(event_data("plotly_deselect", source = "outliers"), {
  req(vals$data)
  keys(NULL)
  vals$data_keep <- vals$data
  vals$data_exclude <- NULL
  plotlyProxy("scatterplot", session) %>%
    plotlyProxyInvoke("restyle",
                      #mode = "markers",
                      # could also do list(marker = list(size = input$marker))
                      # but that overwrites the existing marker definition
                      # https://github.com/plotly/plotly.js/issues/1866#issuecomment-314115744
                      list(marker.line = list(color = 'grey',
                                              width = 2)))
  
})

output$scatterplot <- renderPlotly({
  req(vals$data,
      input$xAxisSelector,
      input$yAxisSelector,
      input$colorBySelector)
  
  x <- input$xAxisSelector
  y <- input$yAxisSelector
  dat <- vals$data[vals$key,]
  
  
  
  name <- "datapoints"
  if (input$colorBySelector != "Do not color") {
    color <-  factor(dat[, input$colorBySelector], ordered = FALSE)
    if (is.factor(dat[, input$colorBySelector])) {
      name <- NULL
    }
  } else{
    color <- NULL
  }
  
  
  model <- regression_model(dat, x, y)
  
  
  scatterplot <- dat %>%
    plot_ly(
      x = dat[, x],
      y = dat[, y],
      key = vals$key,
      color = color,
      source = "outliers"
    )
  scatterplot <- add_markers(scatterplot,
                             marker = list(size = 10, line = list(color = "grey",
                                                                  width = 2)),
                             name = name) %>%
    layout(showlegend = TRUE) %>%
    event_register("plotly_click")
  
  scatterplot <-
    add_lines(
      scatterplot,
      data = model,
      x = ~ x,
      y = ~ yhat,
      color = I("red"),
      key = NULL,
      name = "linear model"
    )
  
  xax <- list(title = x)
  yax <- list(title = y)
  
  scatterplot <-
    scatterplot %>% layout(xaxis = xax, yaxis = yax) %>% onRender(js, data = list(x = "TraceMapping",
                                                                                  ns = sessionval))
  
  
  return(scatterplot)
  
})

output$boxplot <- renderPlotly({
  req(vals$data,
      input$xAxisSelector,
      input$yAxisSelector,
      input$colorBySelector)
  x <- input$xAxisSelector
  y <- input$yAxisSelector
  
  dat <- vals$data[vals$key,]
  
  name <- "datapoints"
  if (input$colorBySelector != "Do not color") {
    color <- NULL
    if (is.factor(dat[, input$colorBySelector])) {
      color <- dat[, input$colorBySelector]
    }
  } else{
    color <- NULL
  }
  xax <- list(title = x)
  yax <- list(title = y)
  
  if (is.numeric(dat[, x])) {
    varx <- factor(round(dat[, x]))
  } else{
    varx <- dat[, x]
  }
  boxplot <-
    dat %>% plot_ly(
      type = 'box',
      mode = "markers",
      y = dat[, y],
      x = varx,
      boxpoints = 'outliers',
      source = "outliers",
      color = color,
      key = vals$key,
      line = list(color = 'rgb(7,40,89)'),
      marker = list(color = "red")
    ) %>%
    layout(boxmode = "group",
           xaxis = xax,
           yaxis = yax)
  
})
output$click <- renderPrint({
  d <- event_data("plotly_click", source = "outliers")
  
  if (is.null(d))
    "click events appear here (double-click to clear)"
  else
    d
})

observeEvent(input$delete_outliers, {
  #update reactive datasets with the data that we keep after deleting outliers
  if (input$outliers_data == "Patient data") {
    datasets$constant_data <- vals$data_keep
    datasets$visit_data <-
      datasets$visit_data[datasets$visit_data[, input$choose_id] %in% datasets$constant_data[, input$choose_id],]
    vals$data_exclude <- NULL
  } else if (input$outliers_data == "Visit data") {
    datasets$visit_data <- vals$data_keep
    datasets$constant_data <-
      datasets$constant_data[datasets$constant_data[, input$choose_id] %in% datasets$visit_data[, input$choose_id],]
    vals$data_exclude <- NULL
  }
  
})
