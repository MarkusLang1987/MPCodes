hc_col <- function(x){
    data <- as.data.frame(round(prop.table(table(x)) * 100, 2))[2]
    row.names(data) <- sort(unique(x))
    return(data)
}
