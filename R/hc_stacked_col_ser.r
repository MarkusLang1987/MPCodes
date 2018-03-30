hc_stacked_col_ser <- function(x, Label = FALSE, columnname) {
    for(i in 1:ncol(x)) {
        if(i == 1) {
            ergframe <- data.frame(matrix(nrow = length(unique(x[[1]])), ncol = 0))
        } else {}
        data <- x %>%
            group_by(x[[i]]) %>%
            summarise (n = n()) %>%
            mutate(freq = round(n / sum(n) * 100, 2))
        data <- data[3]
        ergframe <- merge(ergframe, data, by="row.names", all = TRUE)
        ergframe <- ergframe[-1]
        for(i in 1:ncol(ergframe)) {
            colnames(ergframe)[[i]] <- paste0("freq", i)
        }
    }

    ergframe <- data.frame(t(ergframe))
    row.names(ergframe) <- colnames(x)
    ergframe[is.na(ergframe)] <- 0
    if(Label == TRUE) {
        if(ncol(ergframe) == length(columnname)) {
            colnames(ergframe) <- columnname
        } else {warning("Spaltenzahl und Vektorlaenge nicht identisch")}
    } else {}

    series <- gather(ergframe) %>%
        group_by(name = key) %>%
        do(data = .$value)
    series <- list_parse(series)

    return(series)

}
