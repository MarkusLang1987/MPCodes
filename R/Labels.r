mp_belabelung <- function(data) {
    namelist <- list()
    for(i in 1:ncol(data)) {
        data <- as.data.frame(unique(data[,i]))
        colnames(data) <- NULL
        namelist[[i]] <- data
    }
    return(namelist)
}
