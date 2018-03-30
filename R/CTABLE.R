#' Create a custom Table like in SPSS
#'
#' Take a dataframe and index them by number.
#' @param: data the dataset
#' @param: varx variable on the x-axis of the table
#' @param: vary variable on the y-axis of the table
#' @param: Gewichtung You can use weights on the Output
#' @param: varGewicht Vector, where the weights are stored
#' @export

mp_CTable <- function(data, varx, vary, Gewichtung = FALSE, varGewicht){

    a = 1
    b = 2
    frqtab <- table(data[[varx]], data[[vary]])
    proptab <- round(prop.table(frqtab, 2), 2)

    Ergframe <- data.frame(matrix(nrow = nrow(frqtab), ncol = ncol(frqtab) * 2))

    for(i in 1:ncol(frqtab)) {

        x = as.data.frame(frqtab[,i])
        y = as.data.frame(proptab[,i])

        Ergframe[a:b] <- cbind(x,y)

        colnames(Ergframe)[a] <- colnames(frqtab)[i]
        colnames(Ergframe)[b] <- colnames(proptab)[i]

        for(i in 1:nrow(frqtab)) {
            row.names(Ergframe)[i] <- row.names(frqtab)[i]
        }

        a = a + 2
        b = b + 2
    }

    if(Gewichtung == TRUE){
        Gewicht <- aggregate(data[[varGewicht]], by=list(data[[varx]]), FUN=mean, na.rm = TRUE)

        for(i in 1:nrow(Ergframe)) {
            Ergframe[i,] <- Ergframe[i,] * Gewicht[i,2]
            for(i in 1:ncol(Ergframe)){
                if(i %% 2 != 0) {
                    Ergframe[,i] <- round(Ergframe[,i],0)
                } else {
                    Ergframe[,i] <- round(Ergframe[,i],2)
                }
            }
        }
    } else {}

    return(Ergframe)
}



