#Server module for building panel data

output$choose_panel_max_vars <-
  renderUI({
    #UI element to choose variables for panel dataset aggregate by max
    req(input$panel_timestamp_var,!is.null(input$aggregate))
    if (!is.null(original_data())) {
      if (input$aggregate == TRUE) {
        cols <-
          names(cbind(select_if(
            datasets$nonconstant_data, is.numeric
          )))
        selectizeInput(
          "panel_max_vars",
          "Choose visit variables to aggregate by max: ",
          multiple = T,
          choices = cols,
          selected = NULL
        )
      } else{
        cols <- colnames(datasets$nonconstant_data)
        
        selectizeInput(
          "panel_vars",
          "Choose visit variables: ",
          multiple = T,
          choices = cols,
          selected = NULL
        )
      }
    }
    
  })
output$aggregate <-
  renderUI({
    #UI element to choose variables for panel dataset aggregate by max
    req(input$panel_timestamp_var)
    if (is.numeric(datasets$visit_data[, input$panel_timestamp_var])) {
      checkboxInput(
        "aggregate",
        "Cutoff timepoints and aggregate data",
        value = FALSE,
        width = NULL
      )
    }
    
    
  })

output$choose_panel_sum_vars <-
  renderUI({
    #UI element to choose variables for panel dataset aggregate by sum
    req(input$panel_timestamp_var, input$aggregate)
    
    if (!is.null(original_data())) {
      if (is.numeric(datasets$visit_data[, input$panel_timestamp_var]) &
          input$aggregate == TRUE) {
        cols <-
          names(cbind(select_if(
            datasets$nonconstant_data, is.numeric
          )))
        
        selectizeInput(
          "panel_sum_vars",
          "Choose visit variables to aggregate by sum: ",
          multiple = T,
          choices = cols,
          selected = NULL
        )
      }
    }
  })

output$choose_panel_mean_vars <-
  renderUI({
    #UI element to choose variables for panel dataset aggregate by mean
    req(input$panel_timestamp_var, input$aggregate)
    
    if (!is.null(original_data())) {
      if (is.numeric(datasets$visit_data[, input$panel_timestamp_var]) &
          input$aggregate == TRUE) {
        cols <-
          names(cbind(select_if(
            datasets$nonconstant_data, is.numeric
          )))
        
        selectizeInput(
          "panel_mean_vars",
          "Choose visit variables to aggregate by mean: ",
          multiple = T,
          choices = cols,
          selected = NULL
        )
      }
    }
  })

output$choose_panel_categorical_vars <- renderUI({
  req(input$panel_timestamp_var, input$aggregate)
  
  if (!is.null(original_data())) {
    if (is.numeric(datasets$visit_data[, input$panel_timestamp_var]) &
        input$aggregate == TRUE) {
      data <- cbind(select_if(datasets$nonconstant_data, is.factor))
      cols <- names(data)
      selectizeInput(
        "panel_categorical_vars",
        "Choose visit variables to aggregate by category (TRUE or FALSE): ",
        multiple = T,
        choices = cols,
        selected = NULL
      )
      
    }
  }
})
output$nonlogical_levels <- renderUI({
  req(input$panel_categorical_vars)
  if (!is.nothing(input$panel_categorical_vars)) {
    data <- datasets$nonconstant_data
    data <- data.frame(data[, c(input$panel_categorical_vars)])
    if (length(input$panel_categorical_vars) == 1) {
      colnames(data) <- input$panel_categorical_vars
    }
    nonlogical <-
      cbind(select_if(data, function(col)
        sum(levels(as.factor(col)) == TRUE) == 0 &
          sum(levels(as.factor(col)) == FALSE) == 0))
    #nonlogical <- as.data.frame(nonlogical)
    if (length(nonlogical) > 0) {
      nonlogicalinput <- list()
      for (i in 1:length(nonlogical)) {
        nonlogicalinput[[i]] <-
          selectizeInput(
            paste("nonlogical_levels_", as.character(i), sep = ""),
            label = paste("choose level to mark TRUE for ", colnames(nonlogical)[i]),
            choices = levels(nonlogical[, i]),
            multiple = TRUE
          )
      }
      number_of_inputs <- reactive({
        length(nonlogicalinput)
      })
      return(nonlogicalinput)
    }
  }
})
output$choose_panel_constant_vars <-
  renderUI({
    #UI element to choose constant variables for panel dataset
    req(input$panel_timestamp_var)
    
    if (!is.null(original_data())) {
      if (is.numeric(datasets$visit_data[, input$panel_timestamp_var])) {
        #find time variable columns
        cols <-
          colnames(datasets$constant_data[setdiff(names(datasets$constant_data) , input$choose_id)])
        selectizeInput(
          "panel_constant_vars",
          "Choose constant variables to add to the panel data: ",
          multiple = T,
          choices = cols,
          selected = NULL
        )
      }
    }
  })

output$choose_timestamp_var <-
  renderUI({
    #UI element to choose time var for panel dataset
    if (!is.null(original_data())) {
      #find time variable columns
      cols1 <-
        names(cbind(select_if(
          datasets$nonconstant_data, is.numeric
        )))
      cols2 <-
        names(cbind(select_if(datasets$nonconstant_data, is.Date)))
      cols3 <-
        names(cbind(select_if(
          datasets$nonconstant_data, is.ordered
        )))
      cols4 <-
        names(cbind(select_if(
          datasets$nonconstant_data, is.POSIXct
        )))
      cols5 <-
        names(cbind(select_if(
          datasets$nonconstant_data, is.integer
        )))
      cols <- c(cols1, cols2, cols3, cols4, cols5)
      selectizeInput(
        "panel_timestamp_var",
        "Choose timestamp: ",
        multiple = F,
        choices = cols,
        selected = NULL
      )
    }
  })
output$create_timestamp <-
  renderUI({
    #Text input to create timestamp (needs to be a numeric vector)
    if (!is.null(original_data())) {
      req(input$panel_timestamp_var, input$aggregate)
      if (exists(input$panel_timestamp_var, where = datasets$visit_data)) {
        if (is.numeric(datasets$visit_data[, input$panel_timestamp_var]) &
            input$aggregate == TRUE) {
          min <-
            as.character(min(datasets$visit_data[, input$panel_timestamp_var]))
          median <-
            as.character(median(datasets$visit_data[, input$panel_timestamp_var]))
          max <-
            as.character(max(datasets$visit_data[, input$panel_timestamp_var]))
          value <- paste(min, median, max, sep = ", ")
          textInput(
            "panel_timestamp",
            "Choose time points to create the timestamp from numeric variable (enter the numbers separated by comma such as: 0,3,6,9,12,24,...,180): ",
            value = value
          )
        }
      }
    }
    
  })


panel_data <- eventReactive(input$create_panel, {
  withProgress(value = 0.5,
               message = "Preparing data",
               detail = "...",
               {
                 if (is.null(original_data()))
                   return(NULL)
                 
                 visit <- datasets$visit_data
                 visit$code <- visit[, input$choose_id]
                 showNotification("Unique identifier variable has been renamed <code>")
                 if (is.numeric(visit[, input$panel_timestamp_var])) {
                   if (input$aggregate == TRUE) {
                     if (input$panel_timestamp != "") {
                       if (any(is.na(as.numeric(unlist(
                         strsplit(input$panel_timestamp, ",")
                       ))))) {
                         output$invalid_panel_timestamp <-
                           renderPrint(print("Please type only comma delimited numbers: 1,2,3"))
                       }else{
                         stamp <-
                           cut(
                             visit[, input$panel_timestamp_var],
                             breaks = as.numeric(unlist(
                               strsplit(input$panel_timestamp, ",")
                             ))
                             ,
                             include.lowest = TRUE,
                             right = FALSE,
                             ordered_result = TRUE,
                             dig.lab = 10
                           )
                         output$invalid_panel_timestamp <- NULL
                       }
                     } else{
                       stamp <- NULL
                       output$enter_time_points <-
                         renderPrint(print(
                           "Please type cut-off time points as comma delimited numbers: 1,2,3"
                         ))
                     }
                     
                     if (!is.null(stamp)) {
                       output$enter_time_points <- NULL
                       
                       #stamp[is.na(stamp)] <- 0
                       visit$timestamp <- stamp
                       if (any(is.na(visit$timestamp))) {
                         visit <- visit[!is.na(visit$timestamp),]
                       }
                       if (!is.null(input$panel_max_vars)) {
                         max_per_time <- suppressWarnings(
                           aggregate(
                             . ~ code + timestamp,
                             data = visit[, c("code", "timestamp", input$panel_max_vars)],
                             FUN = max,
                             na.rm = T,
                             na.action = na.pass
                           )
                         )
                         max_per_time <-
                           do.call(data.frame,
                                   lapply(max_per_time, function(x)
                                     replace(x, is.infinite(x), NA)))
                       } else{
                         max_per_time <- visit[, c("code", "timestamp")]
                       }
                       
                       if (!is.null(input$panel_sum_vars)) {
                         sum_per_time <- aggregate(
                           . ~ code + timestamp,
                           data = visit[, c("code", "timestamp", input$panel_sum_vars)],
                           FUN = sum,
                           na.rm = T,
                           na.action = na.pass
                         )
                       } else{
                         sum_per_time <- visit[, c("code", "timestamp")]
                       }
                       
                       if (!is.null(input$panel_mean_vars)) {
                         mean_per_time <- aggregate(
                           . ~ code + timestamp,
                           data = visit[, c("code", "timestamp", input$panel_mean_vars)],
                           FUN = mean,
                           na.rm = T,
                           na.action = na.pass
                         )
                       } else{
                         mean_per_time <- visit[, c("code", "timestamp")]
                       }
                       
                       setProgress(value = 0.5, detail = "Aggregating...")
                       
                       if (!is.null(input$panel_constant_vars)) {
                         constant_per_time <-
                           visit[, c("code", "timestamp", input$panel_constant_vars)]
                       } else{
                         constant_per_time <- visit[, c("code", "timestamp")]
                       }
                       if (!is.nothing(input$panel_categorical_vars)) {
                         category_per_time <-
                           as.data.frame(visit[, c("code",
                                                   "timestamp",
                                                   input$panel_categorical_vars)])
                         number_inputs <-
                           length(cbind(
                             select_if(as.data.frame(category_per_time[, input$panel_categorical_vars]), function(col)
                               sum(levels(as.factor(col)) == TRUE) == 0 &
                                 sum(levels(as.factor(col)) == FALSE) == 0)
                           ))
                         inputs <- NULL
                         if (number_inputs != 0) {
                           for (i in 1:number_inputs) {
                             inputs <-
                               append(inputs, input[[paste("nonlogical_levels_", as.character(i), sep = "")]])
                           }
                         }
                         
                         #
                         category_per_time <- category_per_time %>%
                           dplyr::group_by(code, timestamp) %>%
                           dplyr::mutate_all( ~ if_else(
                             any(as.character(.x) %in% inputs) |
                               +any(as.character(.x) == "TRUE"),
                             "TRUE",
                             "FALSE"
                           ))
                         category_per_time <-
                           as.data.frame(lapply(category_per_time[setdiff(names(category_per_time), "code")], factor))
                       } else{
                         category_per_time <- visit[, c("code", "timestamp")]
                       }
                       
                       
                       
                       data <- left_join(
                         max_per_time,
                         left_join(
                           sum_per_time,
                           left_join(
                             mean_per_time,
                             left_join(
                               category_per_time,
                               constant_per_time,
                               by = c("code", "timestamp")
                             ),
                             by = c("code", "timestamp")
                           ),
                           by = c("code", "timestamp")
                         ),
                         by = c("code", "timestamp")
                       )
                     } else{
                       data <- NULL
                     }
                     
                     if (!is.null(data)) {
                       data <- data[!duplicated(data[, c("code", "timestamp")]),]
                     }
                   } else{
                     visit$timestamp <- as.ordered(visit[, input$panel_timestamp_var])
                     data <-
                       visit[, c("code",
                                 "timestamp",
                                 input$panel_vars,
                                 input$panel_constant_vars)]
                     data <-
                       data[!duplicated(data[, c("code", "timestamp")]),]
                   }
                 } else{
                   visit$timestamp <- as.ordered(visit[, input$panel_timestamp_var])
                   data <-
                     visit[, c("code",
                               "timestamp",
                               input$panel_vars,
                               input$panel_constant_vars)]
                   data <- data[!duplicated(data[, c("code", "timestamp")]),]
                   print(str(data))
                 }
                 
                 
                 data <- data %>% mutate_if(is.factor,
                                            fct_explicit_na,
                                            na_level = "Missing")
                 
                 setProgress(value = 1, detail = "done!")
               })
  data
})

#render table
output$test_panel <-  DT::renderDT({
  if (is.null(panel_data()))
    return(NULL)
  
  panel_data()
}, options = list(pageLength = 5))

output$panel_options <- renderUI({
  #time trend plot
  
  if (!is.null(panel_data())) {
    cols <- names(cbind(select_if(panel_data(), is.numeric)))
    facs <-
      names(cbind(select_if(panel_data()[setdiff(names(panel_data()), "timestamp")], is.factor)))
    integ <-
      apply(panel_data()[, cols, drop = F], 2, function(x) {
        #if a column is num but has less than 4 distinct values, turn to ordered factor
        length(unique(x)) <= 4 &&
          sum(table(x) == 1) < length(unique(x)) / 2
      })
    integ <- names(integ)[integ]
    
    tagList(
      selectizeInput(
        "panel_y",
        "Choose y axis: ",
        multiple = F,
        choices = colnames(panel_data()),
        selected = NULL
      ),
      selectizeInput(
        "panel_x",
        "X axis:",
        multiple = F,
        choices = colnames(panel_data()),
        selected = NULL
      ),
      selectizeInput(
        "panel_color",
        "Color by:",
        multiple = F,
        choices = c("Do not color", facs, integ),
        selected = "Do not color"
      ),
      selectizeInput(
        "panel_shape",
        "Point Shapes by:",
        multiple = F,
        choices = c("No shapes", facs, integ),
        selected = "No shapes"
      ),
      selectizeInput(
        "panel_size",
        "Point size by:",
        multiple = F,
        choices = c("No sizes", colnames(panel_data())),
        selected = "No sizes"
      ),
      selectizeInput(
        "panel_wrap",
        "Wrap: ",
        multiple = F,
        choices = c("No wrap", facs[!facs == "code"], integ),
        selected = NULL
      ),
    )
  }
  
})


output$time_trend <- renderPlotly({
  #render time trend plot
  shinyjs::disable("panel_x")
  shinyjs::disable("panel_size")
  shinyjs::disable("panel_wrap")
  shinyjs::disable("panel_shape")
  shinyjs::enable("panel_y")
  shinyjs::enable("panel_color")
  
  if (!is.null(panel_data())) {
    req(input$panel_color, input$panel_y)
    gd <- panel_data()
    if (input$panel_color != "Do not color") {
      gd[, input$panel_color] <- as.factor(gd[, input$panel_color])
      mean_per_group <-
        setNames(
          aggregate(
            gd[, input$panel_y] ~ gd[, input$panel_color] + gd$timestamp,
            data = gd,
            FUN = mean,
            na.rm = T,
            na.action = na.pass
          ),
          c(input$panel_color, "timestamp", input$panel_y)
        )
      
      timetrend <-
        gd %>% highlight_key( ~ code) %>% ggplot(.,
                                                 aes_string(
                                                   x = "timestamp",
                                                   y = input$panel_y,
                                                   color = input$panel_color
                                                 ),
                                                 key = code)
      timetrend <-
        timetrend + geom_line(aes(group = code), size = .2,  alpha = .5) +
        theme_bw()
      timetrend <-
        timetrend + geom_line(
          data = mean_per_group,
          group = 1,
          size = 2,
          alpha = 1
        )
      
    } else{
      panelmean <-
        setNames(
          aggregate(
            gd[, input$panel_y] ~ gd$timestamp,
            data = gd,
            FUN = mean,
            na.rm = T,
            na.action = na.pass
          ),
          c("timestamp", input$panel_y)
        )
      
      timetrend <-
        gd %>% highlight_key( ~ code) %>% ggplot(., aes_string(x = "timestamp", y = input$panel_y), key = code) +
        geom_line(aes_string(group = "code"),
                  size = .2,
                  alpha = .5) +
        theme_bw()
      timetrend <-
        timetrend + geom_line(
          data = panelmean,
          group = 1,
          size = 2,
          alpha = 1
        )
      
    }
    return(
      ggplotly(timetrend, source = "timetrend") %>% highlight(
        on = "plotly_click",
        off = "plotly_doubleclick",
        selected = attrs_selected(showlegend = TRUE)
      )
    )
    dev.off()
    
  }
})

output$boxtrend <- renderPlotly({
  shinyjs::disable("panel_x")
  shinyjs::disable("panel_size")
  shinyjs::disable("panel_wrap")
  shinyjs::disable("panel_shape")
  shinyjs::enable("panel_y")
  shinyjs::enable("panel_color")
  req(panel_data(), input$panel_color, input$panel_y)
  panel <- panel_data()
  y <- input$panel_y
  colour <- NULL
  if (input$panel_color != "Do not color") {
    colour <- input$panel_color
    panel[, input$panel_color] <-
      as.factor(panel[, input$panel_color])
    
  }
  boxtrend <-
    panel %>% ggplot(., aes_string("timestamp", y, fill = colour)) +
    geom_boxplot(width = 0.1, outlier.colour = "blue")
  #theme_classic()
  
  return(ggplotly(boxtrend))
})


output$downloadPanelData <-
  downloadHandler(
    filename = function() {
      #download button
      #visit <- visit_data()
      paste("panel", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(panel_data(),
                file, row.names = FALSE)
    }
  )

output$layeredplot <- renderPlotly({
  shinyjs::enable("panel_x")
  shinyjs::enable("panel_size")
  shinyjs::disable("panel_wrap")
  shinyjs::enable("panel_shape")
  shinyjs::enable("panel_y")
  shinyjs::enable("panel_color")
  
  req(panel_data(),
      input$panel_x,
      input$panel_y,
      input$panel_color,
      input$panel_shape)
  panel <- panel_data()
  x <- input$panel_x
  y <- input$panel_y
  if (input$panel_color != "Do not color") {
    panel[, input$panel_color] <- as.factor(panel[, input$panel_color])
  }
  if (input$panel_shape != "No shapes") {
    panel[, input$panel_shape] <- as.factor(panel[, input$panel_shape])
  }
  layeredplot <-
    panel %>% highlight_key( ~ code) %>% ggplot(
      .,
      aes_string(x, y, frame = "timestamp"),
      xlab = x,
      ylab = y,
      key = code
    ) +
    geom_point() +
    theme(legend.position = "bottom")
  if (input$panel_color != "Do not color") {
    layeredplot <-
      (layeredplot + aes_string(colour = input$panel_color))
  }
  if (input$panel_shape != "No shapes") {
    layeredplot <- (
      layeredplot + aes_string(shape = input$panel_shape) +
        scale_shape_manual(values = c(1:25))
    )
  }
  if (input$panel_size != "No sizes") {
    layeredplot <- (layeredplot + aes_string(size = input$panel_size))
  }
  return(
    ggplotly(layeredplot, source = "layeredplot") %>% highlight(
      on = "plotly_click",
      off = "plotly_doubleclick",
      selected = attrs_selected(showlegend = TRUE)
    )
  )
  dev.off()
})



output$facetedplot <- renderPlotly({
  shinyjs::disable("panel_x")
  shinyjs::disable("panel_size")
  shinyjs::enable("panel_wrap")
  shinyjs::disable("panel_shape")
  shinyjs::enable("panel_y")
  shinyjs::enable("panel_color")
  
  req(panel_data(),
      input$panel_wrap,
      input$panel_y,
      input$panel_color)
  panel <- panel_data()
  x <- input$panel_wrap
  y <- input$panel_y
  if (input$panel_color != "Do not color") {
    panel[, input$panel_color] <- as.factor(panel[, input$panel_color])
    color <- input$panel_color
  } else{
    color <- NULL
  }
  
  facetedplot <-
    panel %>% highlight_key( ~ code) %>% ggplot(.,
                                                aes_string("timestamp", y, group = "code", color = color),
                                                key = code) +
    geom_line() +
    geom_point()
  
  if (x != "No wrap") {
    facetedplot <- facetedplot +
      facet_wrap(panel[, x], ncol = 2)
  }
  
  facetedplot <- ggplotly(facetedplot, source = "facetedplot") %>%
    highlight(on = "plotly_click",
              off = "plotly_doubleclick",
              selected = attrs_selected(showlegend = TRUE))
  
  return(facetedplot)
  dev.off()
})


#keys <- reactiveVal()
data_clicked <- reactiveVal()
#observeEvent(event_data("plotly_afterplot", source = "outliers", priority = "event"), {
#   print(input$TraceMapping)
# })


subset_data <- function(source) {
  observeEvent(event_data("plotly_click", source = source, priority = "event"),
               {
                 req(panel_data(),
                     event_data("plotly_click", source = source)$key)
                 #print(event_data("plotly_click", source = source)$key)
                 panel <- panel_data()
                 is_selected <- NULL
                 key_new <- event_data("plotly_click", source = source)$key
                 data_clicked(panel[which(panel$code == key_new),])
                 
               })
}
subset_data("timetrend")
subset_data("layeredplot")
subset_data("facetedplot")

output$display_table <- renderUI({
  #req(data_clicked())
  selectizeInput(
    "display_table",
    "Choose variables to display in the table: ",
    multiple = T,
    choices = colnames(panel_data()),
    selected = colnames(panel_data())[1:5]
  )
})
output$table_clicked <- renderDT({
  as.data.frame(data_clicked()[, input$display_table])
}, options = list(pageLength = 5))
