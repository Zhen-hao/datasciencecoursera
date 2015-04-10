corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    table <- complete(directory,)
    truth.value <- (table[,2] > threshold)
    table <- table[truth.value,]
    

    pool <- data.frame(sulfate = numeric(), nitrate = numeric())
    for (i in table[,1]){
        comp.sub <- read(directory,i)[,2:3]
        pool <- rbind(pool,comp.sub)
    }   
    cor(pool[,1],pool[,2], use = "pairwise.complete.obs")
}

read <- function(dir,i){
    filelist <- list.files(dir)
    presentwd <- getwd()
    setwd(dir)
    sensordata <- read.csv(filelist[i])
    setwd(presentwd)
    sensordata
}
