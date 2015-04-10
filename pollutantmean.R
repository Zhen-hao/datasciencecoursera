pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
    
    filelist <- list.files(directory)
    
    #vector is the combined list of the observed pollutant value from all sensors
    vect <- numeric()
    
    for (i in id){
        
        #read sensor file
        presentwd <- getwd()
        setwd(directory)
        sensordata <- read.csv(filelist[i])
        setwd(presentwd)
        
        vect <- c(vect,getpollutant(sensordata, pollutant))
    }
    
    mean(vect)
}


getpollutant <- function (sensor.data, pol.name){
    v <- sensor.data[,pol.name]
    bad <- is.na(v)
    v[!bad]
}