complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    
    #    data format:   Date sulfate nitrate ID

    filelist <- list.files(directory)
    
    table <- data.frame(id = integer(), nobs = integer())
    
    for (i in id){
        
        presentwd <- getwd()
        setwd(directory)
        sensordata <- read.csv(filelist[i])
        setwd(presentwd)
        
        s.bad <- is.na(sensordata[,"sulfate"])
        n.bad <- is.na(sensordata[,"nitrate"])
        good <- sensordata[!s.bad & !n.bad,]
        item <- data.frame(id =i,nobs = nrow(good))
        
        table <- rbind(table, item)
    }
    table
}