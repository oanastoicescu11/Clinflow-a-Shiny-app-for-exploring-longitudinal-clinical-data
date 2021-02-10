#This is the main server file where we defined some important functions and workspace 

options(shiny.maxRequestSize=30*1024^2,warn = -1)
function(input, output, session) {
    #track user actions
    observe({
        input$logger
        if(!is.null(input$logger)){
            inputLog <- c(input$logger, as.character(Sys.time()))
            # some input give double values - shorten to one string to fit it in the data table
            if(length(inputLog) == 5) inputLog <- c(paste(input$logger[1:2], collapse = "-"), input$logger[3:4], as.character(Sys.time()))
            # wait till file was updated
            Sys.sleep(0.3)
            write.table(as.data.frame(rbind(inputLog)), "log.csv", sep = ";", append = TRUE,  row.names = FALSE, col.names = FALSE)
        }
    })
    ########################################
    ## creat tmp workspace 
    mydir <- paste0(getwd(), "/tmp/", getTmpString())
    dir.create(mydir, recursive=T)
    
    ## cleaning temp data
    onReactiveDomainEnded(session, function(){
        for(d in dev.list()){
            graphics.off()
        }
        ## if(length(list.files(mydir)) == 0){
        print(paste("Cleaning workspace:", mydir))
        unlink(mydir, recursive=T, force=T)
        ## }
        ## print(names(session$clientData))
        ## print(getwd())
    })
    #Function to remove special characters from column names(from HTPMod)
    nospecial <- function(x){
      gsub("[^[:alnum:]]", ".", x)
    }
    
    source("server-inputdata.R", local = TRUE)
    source("server-preprocessing-constant.R", local = TRUE)
    source("server - missingmap.R", local = TRUE)
    source("server-filter.R", local = TRUE)
    source("server-create new constant var.R", local = TRUE)  
    source("server-outliers.R", local = TRUE)
    source("server-tools-mydata.R", local = TRUE)
   # source("server-tools-analysis.R", local = TRUE)
    source("server-analysis-PCA.R", local = TRUE)
    source("server-analysis-TSNE.R", local = TRUE)
    source("server-analysis-MDS.R", local = TRUE)
    source("server-analysis-SOM.R", local = TRUE)
    source("server-analysis-KMC.R", local = TRUE)
    source("server-analysis-CHARTS.R", local = TRUE)
    
    source("server-panel.R", local = TRUE)
    source("server-categorize.R", local = TRUE)
    source("server-numerize.R", local = TRUE)
    
    source("server-survival.R", local = TRUE)
    
    
}
