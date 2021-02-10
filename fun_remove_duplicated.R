remove_duplicated <- function(data){
    #remove duplicated rows
    if(any(duplicated(data))){
        data <- data[!duplicated(data), ]
    }
    return(data)
}