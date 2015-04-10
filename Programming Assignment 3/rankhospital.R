rankhospital  <- function (state, outcome, num = "best") {
    
    if (!is.element(state, state.abb)) {
        stop("invalid state")
    }
    
    if ((outcome != "heart attack") && (outcome != "heart failure") && (outcome != "pneumonia")){
        stop("invalid outcome")
    }
    
    table.all <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    truth.state <- table.all$State == state
    table.state <- table.all[truth.state,]
    
    if (outcome == "heart attack") {
        
        ranking <- rank.heart.attack(table.state)
        
    }
    
    if (outcome == "heart failure") {
        
        ranking <- rank.heart.failure(table.state)
        
    }
    
    if (outcome == "pneumonia") {
        
        ranking <- rank.pneumonia(table.state)
        
    }
    
    if (num =="best") {
        result <- head(ranking, n = 1L)
    }
    if (num =="worst") {
        result <- tail(ranking, n = 1L)
    }
    
    else{
        result <- ranking[num,] 
    }
    
    result[1,2]
    
    
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