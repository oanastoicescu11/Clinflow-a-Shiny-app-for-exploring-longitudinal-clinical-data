output$uipxyzm <- renderUI({
    #options for charts
    input$visualization_data
    inputData <- inputDataAnalysis()
    myValidation(inputData)
    showNotification("Rendering variables ...")
    
    valx <-
        inputData$nonNumeric ## setdiff(inputData$nonNumeric, input$rowColor)
    vals <- inputData$numericCol
    div(
        selectizeInput(
            "miscx",
            "X variable: ",
            multiple = F,
            choices = setNames(valx, valx)
        ),
        selectizeInput(
            "miscy",
            "Y variable: ",
            multiple = F,
            choices = setNames(vals, vals)
        ),
        selectizeInput(
            "miscz",
            "Z variable: ",
            multiple = F,
            choices = setNames(vals, vals)
        ),
        selectizeInput(
            "miscm",
            "M variable: ",
            multiple = T,
            choices = c(setNames(vals, vals), setNames(valx, valx)),
            selected = head(vals)
        )
    )
})



CHARTS_plotly <- reactive({
    inputData <- inputDataAnalysis()
    req(inputData$data)
    need(!is.null(params()), "Some UI elements are not initialized.")
    data <- inputData$data
    colnames(data) <- nospecial(colnames(data))
    req(input$misct)
    #symbol_list<-params()$symbol_list
    color_list <- params()$color_list
    ggsymbol_list <- params()$ggsymbol_list
    
    # mapped_colors<-params()$mapped_colors
    # mapped_symbols<-params()$mapped_symbols
    symbol <- params()$symbol_column
    color <- nospecial(params()$color_column)
    req(input$rowColor,
        input$miscx,
        input$miscm,
        input$miscz,
        input$miscy)
    if (input$rowColor != "") {
        colorname <- nospecial(input$rowColor)
    } else{
        colorname <- color
    }
    name <- params()$name
    text <- params()$text
    ps <- unlist(strsplit(input$misct, split = "\\|"))
    fill <- colorname
    # if(length(color)<= 1){
    #     fill <- NA
    # }
    x <- nospecial(input$miscx)
    y <- nospecial(input$miscy)
    z <- nospecial(input$miscz)
    m <- nospecial(input$miscm)
    if (ps[1] == "1") {
        ## Plot One Variable: Continuous Y
        shinyjs::enable("miscy")
        shinyjs::disable("miscx")
        shinyjs::disable("miscz")
        shinyjs::disable("miscm")
        if (!is.nothing(y)) {
            pltfun <- get(ps[2])
            p <-
                pltfun(
                    data,
                    x = y,
                    add = "mean",
                    rug = TRUE,
                    color = colorname,
                    fill = colorname,
                    palette = color_list
                )
            if (ps[2] == "ggqqplot") {
                p <- ggplotly(p) %>%
                    style(text = text, hoverinfo = c(x, y))#, traces = c(1,2,3)
            } else{
                p <- ggplotly(p)
            }
            
            return(p)
        } else{
            msgPlot()
        }
    } else if (ps[1] == "4") {
        ## Plot One Variable percentage : Discrete X
        shinyjs::enable("miscx")
        shinyjs::disable("miscy")
        shinyjs::disable("miscz")
        shinyjs::disable("miscm")
        if (!is.nothing(x)) {
            p <- ggplot(data, aes_string(colorname, group = x)) +
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat =
                             "count") +
                geom_text(
                    aes(
                        label = scales::percent(..prop..),
                        y = ..prop..
                    ),
                    stat = "count",
                    vjust = -.5
                ) +
                labs(y = "Percent", fill = fill) +
                scale_y_continuous(labels = scales::percent) +
                facet_grid( ~ data[, x])
            p <- ggplotly(p)
            return(p)
        } else{
            showNotification("X is missing.", type = 'error')
            msgPlot()
        }
    } else if (ps[1] == "2") {
        ## Plot Two Variables: Discrete X & Continuous Y
        shinyjs::enable("miscx")
        shinyjs::enable("miscy")
        shinyjs::disable("miscz")
        shinyjs::disable("miscm")
        if (!is.nothing(x) && !is.nothing(y)) {
            pltfun <- get(ps[2])
            p <-
                pltfun(
                    data,
                    x,
                    y,
                    color = colorname,
                    palette = color_list,
                    add = c("mean_se", "jitter")
                ) +
                labs(y = input$miscy, x = input$miscx)
            if (ps[2] == "ggbarplot" |
                ps[2] == "ggboxplot" | ps[2] == "ggviolin") {
                p <- ggplotly(p) %>%
                    style(text = text, hoverinfo = c(y, x))#, traces = c(4,5,6)
            } else if (ps[2] == "ggline") {
                p <- ggplotly(p) %>%
                    style(text = text, hoverinfo = c(y, x))#, traces = c(1,2,3,7,8,9)
            } else if (ps[2] == "ggstripchart") {
                p <- ggplotly(p) %>%
                    style(text = text, hoverinfo = c(y, x))#, traces = c(1,2,3)
            }
            return(p)
        } else{
            showNotification("X or Y is missing.", type = 'error')
            msgPlot()
        }
    } else if (ps[1] == "3") {
        shinyjs::enable("miscy")
        shinyjs::enable("miscz")
        shinyjs::disable("miscx")
        shinyjs::disable("miscm")
        if (y == z) {
            showNotification("Y and Z are the same column.", type = 'error')
        } else if (!is.nothing(y) && !is.nothing(z)) {
            if (ps[2] == "stat_cor") {
                correlation <- NULL
                for (i in unique(data[, colorname])) {
                    if (length(which(as.factor(data[, colorname]) == i)) >= 3) {
                        correlation[i] <-
                            paste0(
                                i,
                                ": ",
                                "R = ",
                                round(with(
                                    data[which(as.factor(data[, colorname]) == i),], cor.test(data[which(as.factor(data[, colorname]) == i), y], data[which(as.factor(data[, colorname]) == i), z])
                                )$estimate, 2),
                                ", p = ",
                                round(with(
                                    data[which(as.factor(data[, colorname]) == i),], cor.test(data[which(as.factor(data[, colorname]) == i), y], data[which(as.factor(data[, colorname]) == i), z])
                                )$p.value, 8)
                            )
                    } else{
                        correlation[i] <- paste0(i, ": ", "Not enough observations")
                    }
                }
                correlation <-
                    paste0(correlation,
                           sep = " ",
                           collapse = "; ")
                print(correlation)
                #p <- GGally::ggpairs(data[,c(y,z)], aes(color = color),lower = list(continuous = "smooth_lm"))
                p <-
                    ggscatter(
                        data,
                        x = y,
                        y = z,
                        color = colorname,
                        palette = color_list,
                        add = "reg.line",
                        conf.int = TRUE
                    ) +
                    annotate(
                        "text",
                        label = correlation,
                        x = max(data[, y]) * 0.65,
                        y = max(data[, z]),
                        size = 3.5
                    )
                p <- ggplotly(p) #%>%
                #style(text = text, hoverinfo = c(x,y,text))#, traces = c(1,2,3)
                #stat_cor(aes_string(color=colorname))
            } else if (ps[2] == "stat_stars") {
                p <-
                    ggscatter(
                        data,
                        x = y,
                        y = z,
                        color = colorname,
                        shape = colorname,
                        palette = color_list,
                        mean.point = TRUE,
                        ellipse = TRUE,
                        star.plot = TRUE
                    )
                ## + stat_stars(aes_string(color=color))
                p <-  p + labs(x = input$miscy, y = input$miscz)
                p <- suppressWarnings(ggplotly(p))
                return(p)
            } else{
                showNotification("Y or Z is missing.", type = 'error')
                NULL
            }
        }
    } else if (ps[1] == "m") {
        shinyjs::disable("miscy")
        shinyjs::enable("miscm")
        shinyjs::disable("miscx")
        shinyjs::disable("miscz")
        if (!is.nothing(m)) {
            if (length(color) > 1) {
                if (ps[2] == "pairs") {
                    p <- GGally::ggpairs(as.data.frame(data[, m]), aes(color = color))
                    p <-
                        suppressWarnings(
                            ggplotly(p) %>% highlight(on = "plotly_click", off = "plotly_deselect")
                        )
                    
                    return(p)
                }
            } else{
                p <-
                    GGally::ggpairs(data[1:100, m], lower = list(continuous = "smooth"))
                p <-
                    suppressWarnings(
                        ggplotly(p) %>% highlight(on = "plotly_click", off = "plotly_deselect")
                    )
            }
        } else{
            showNotification("Y or M is missing.", type = 'error')
            msgPlot()
        }
    } else{
        msgPlot()
    }
    
})

output$plotly_chart <- renderPlotly({
    CHARTS_plotly()
})