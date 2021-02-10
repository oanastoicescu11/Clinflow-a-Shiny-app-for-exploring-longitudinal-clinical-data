
##########################################################################


my.legend <- function(..., inset=0.01, pch=19, box.col=NA, title.col='black', bg="#EFEFEF"){
    #function for plots legends (from HTPMod)
    legend(..., inset=inset, pch=pch, box.col=box.col, bg=bg, title.col=title.col)
}

is.nothing <- function(x, false.triggers = FALSE) {
    #a validator function
    if (is.function(x))
        return(FALSE)
    return(is.null(x) ||
               length(x) == 0 ||
               all(is.na(x)) ||
               all(x == "") || (false.triggers && all(!x)))
}

getTmpString <- function() {
    #function for naming the temporary folder (from HTPmod)
    a <- do.call(paste0, replicate(5, sample(c(LETTERS, letters), 1, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, 1, TRUE)), sample(LETTERS, 1, TRUE))
}

find_nonconstant_cols <- function(data, ID){
    #function to find the visit cols
    const_in_id <- function(x) anyDuplicated(unique(data[c(ID, x)])[[ID]])
    return(Filter(const_in_id, names(data)))
}

find_constant_cols <- function(data, ID){
    #function to find the patient cols
    const_in_id <- function(x) !anyDuplicated(unique(data[c(ID, x)])[[ID]])
    return(Filter(const_in_id, names(data)))
}

delete_lvl <- function(x){ #funtion to delete a lvl with 0 entries from a factor in the filtered data bc it causes errors in the vizualizations 
    return(as.factor(as.character(x)))
}