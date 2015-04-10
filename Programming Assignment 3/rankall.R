rankall  <- function (outcome, num = "best") {
    
  
    if ((outcome != "heart attack") && (outcome != "heart failure") && (outcome != "pneumonia")){
        stop("invalid outcome")
    }
    
    table.all <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    
    #result <- data.frame(hospital, state)
    
    if (outcome == "heart attack") {
        
        ranking <- lapply(split(table.all, table.all[,7]), rank.heart.attack)
        
    }
    
    finalresult <- data.frame(hospital=character(), state=character())
    
    if (outcome == "heart failure") {
        
        ranking <- lapply(split(table.all, table.all[,7]), rank.heart.failure)
        
    }
    
    if (outcome == "pneumonia") {
        
        ranking <- lapply(split(table.all, table.all[,7]), rank.pneumonia)
        
    }
    
    if (num =="best") {
        result <- lapply(ranking, function (t) {head(t[,c(2,7)],n=1L)})
    }
    if (num =="worst") {
        result <- lapply(ranking, function (t) {tail(t[,c(2,7)],n=1L)})
    }
    
    else{
        result <- lapply(ranking, function (t) {t[num,c(2,7)]})
    }
    
    finalresult <- rbind.fill(result)
    
    #rename(finalresult, c("Hospital.Name"="hospital", "State"=state))
    colnames(finalresult)[1] <- "hospital"
    
    colnames(finalresult)[2] <- "state"

    finalresult
}

rank.heart.attack <- function (statedata) {
    
    data <- statedata[statedata[,11] != "Not Available",]
    data[,11] <- as.numeric(data[,11])
    ranking <- data[order(data[,11], data[,2]),]
    ranking
}

rank.heart.failure <- function (statedata) {
    
    data <- statedata[statedata[,17] != "Not Available",]
    data[,17] <- as.numeric(data[,17])
    ranking <- data[order(data[,17], data[,2]),]
    ranking
}

rank.pneumonia <- function (statedata) {
    
    data <- statedata[statedata[,23] != "Not Available",]
    data[,23] <- as.numeric(data[,23])
    ranking <- data[order(data[,23], data[,2]),]
    ranking
}