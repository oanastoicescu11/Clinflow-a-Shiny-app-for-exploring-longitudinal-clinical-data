

original_data <- reactive({
    #input data
    if (input$own_data == TRUE) {
        inFile <- input$file1
        if (is.null(inFile)) {
            return(NULL)
        }else{
            mtry <- try(read.table(inFile$datapath, header = TRUE,sep = input$sep, stringsAsFactors = FALSE), 
                        silent = TRUE)
            
            if (class(mtry) != "try-error") {
                df <- data.frame(read.table(inFile$datapath, header = TRUE,sep = input$sep, stringsAsFactors = FALSE))
            } else {
                df <- NULL
                showNotification("Your data frame's format is incorrect. Choose the appropriate separator in the left panel or upload a different dataset.", 
                                 duration = NULL, type = "error")
            }
            if(!is.null(df)){
                if(is.null(ncol(df))|ncol(df)<=1){
                    showNotification("Your data frame's format is incorrect. Upload a valid dataframe and choose the appropriate separator in the left panel.", 
                                     duration = NULL, type = "error")
                }
            }
        }
        
        
    }else{
        df <- survival::pbcseq
    }
    return(df)
})

output$upload_options <- renderUI(if (input$own_data == TRUE) {
    tagList(
        radioButtons(
            "sep",
            "Separator",
            choices = c(
                Comma = ",",
                Semicolon = ";",
                Tab = "\t"
            ),
            selected = ","
        ),
        tags$hr(),
        
        # Input: Select a file ----
        fileInput(
            "file1",
            "Choose CSV Files",
            multiple = TRUE,
            accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
            )
        )
    )
} else{
    helpText(
        "Demo data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver,
                available in R package <survival>", tags$a(tags$a(href="https://aasldpubs.onlinelibrary.wiley.com/doi/abs/10.1002/hep.1840200120", "Details here."))
    )
})
#render table and summaries
output$table_input <- DT::renderDT({
    if(!is.null(original_data())){
        original_data()
    }else{
        NULL
    }
}, options = list(pageLength = 5))


output$summary_input <- renderPrint({
    summary(original_data())
})

output$structure_input <- renderPrint({
    str(original_data())
})

output$choose_id <- renderUI(
    selectizeInput(
        "choose_id",
        label = "Choose the column for the unique patient identifier",
        choices = colnames(original_data()),
        multiple = FALSE
    )
)
