mp_belabelung <- function(data) {
    namelist <- list()
    for(i in 1:ncol(data2)) {
        data <- as.data.frame(unique(data2[,i]))
        colnames(data) <- NULL
        namelist[[i]] <- data
    }
    return(namelist)
}
