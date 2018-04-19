mp_belabelung <- function(data) {
    namelist <- list()
    for(i in 1:ncol(data2)) {
        namelist[[i]] <- unique(data2[,i])
    }
    return(namelist)
}