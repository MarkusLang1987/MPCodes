####

mp_Vlookup <- function(Suchvektor, ## Zahl oder Zahlen (in Vektorform, die man sucht)
                     Suchtabelle, ## In Welcher tabelle will man die Zahlen des Suchvektors finden
                     Suchspalte, ## In Welcher Spalte befinden sich die zu suchenden Werte?
                     Ergspalte, ## Wie lautet der Spaltenindex?
                     newDF = FALSE){ ## Moechte man nur die Ergebnisse oder Ergebnisse und Suchvektor zusammen haben? Standardmaessig nein

    SubDF <- subset(Suchtabelle, as.numeric(t(Suchtabelle[Suchspalte])) %in% as.numeric(t(Suchvektor)))
    Logik <- as.data.frame(as.numeric(t(Suchvektor)) %in% as.numeric(t(SubDF[1])))
    CountFehlwerte <- length(Logik[Logik == FALSE])

    print(paste("Anzahl der nicht in Suchvektor vorhandenen Werte:", CountFehlwerte))

    if(newDF == FALSE) {
        if(Ergspalte > ncol(Suchtabelle)) {
            stop(paste("Suchspalte kann nicht groesser als Anzahl der Spalten in Suchtabelle sein"))
        }

        NewDF <- SubDF[Ergspalte]
        colnames(NewDF)[1] <- "Suchwert"
        return(NewDF)

    } else if (newDF == TRUE) {
        if(Ergspalte > ncol(Suchtabelle)) {
            stop(paste("Suchspalte kann nicht groesser als Anzahl der Spalten in Suchtabelle sein"))

        }

        Ergframe <- data.frame(matrix(nrow = length(Suchvektor), ncol = 3))
        Ergframe[1] = Suchvektor
        Ergframe[2] = Logik

        for (i in 1:nrow(Ergframe)) {
            if(Ergframe[i, 2] == FALSE) {
                Ergframe[i, 3] = NA
            } else {Ergframe[i, 3] = Ergframe[i, 1]}
        }


        Ergframe <- na.omit(Ergframe)
        NewDF <- cbind(Ergframe[3], SubDF[Ergspalte])
        colnames(NewDF)[1] <- "Suchwert"
        colnames(NewDF)[2] <- "Ergebnis"

        return(NewDF)

    } else {}

}

a <- c(1,2,3,4,5,6,7,8,9,10)
b <- c("bla", "muh", "wee", "wup", "huh", "wuschi", "blub", "aua", "yihaa", "buhu")
c <- c(100, 40, 30, 10, 20, 20, 30, 150, 90, 10)
df1 <- data.frame(a,b,c)


mp_Vlookup(c(1,2,3,7,11,13), df1, 1, 2, newDF = FALSE)
mp_Vlookup(c(1,2,3,7,11,13), df1, 1, 2, newDF = TRUE)
