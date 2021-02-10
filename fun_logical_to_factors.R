logical_to_factors <- function(data){
    for(i in 1:ncol(data)){
        if(is.logical(data[,i])) { 
            data[,i] <- as.factor(data[,i])
        }

    }
    return(data)
}