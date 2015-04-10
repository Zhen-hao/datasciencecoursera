best <- function (state, outcome){
    
    if (!is.element(state, state.abb)) {
        stop("invalid state")
    }
    
    if ((outcome != "heart attack") && (outcome != "heart failure") && (outcome != "pneumonia")){
        stop("invalid outcome")
    }
    
    table.all <- read.csv("outcome-of-care-measures.csv")
    
    truth.state <- table.all$State == state
    table.state <- table.all[truth.state,]
    
    if (outcome == "heart attack") {
        
        result <- heart.attack(table.state)
        
    }
    
    if (outcome == "heart failure") {
        
        result <- heart.failure(table.state)
        
    }
    
    if (outcome == "pneumonia") {
        
        result <- pneumonia(table.state)
        
    }
    
    showname(result)
}

heart.attack <- function (statedata) {
    
    ## as.numeric(as.character(ndata[,11]))
    ## ndata <- data[data[,11] != "Not Available",]
    data <- statedata[statedata[,11] != "Not Available",]
    data[,11] <- as.numeric(as.character(data[,11]))
    m <- min(data[,11])
               ## Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    final.list <- data[(data[,11]) == m,]
                ## $Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min
    final.list
    ## showname(final.list)
}

showname <- function (list) {
    min(as.character(list$Hospital.Name))
}


heart.failure <- function (statedata) {
    
    data <- statedata[statedata[,17] != "Not Available",]
    data[,17] <- as.numeric(as.character(data[,17]))
    m <- min(data[,17])
    
    final.list <- data[(data[,17]) == m,]
    final.list
}

pneumonia <- function (statedata) {
    
    data <- statedata[statedata[,23] != "Not Available",]
    data[,23] <- as.numeric(as.character(data[,23]))
    m <- min(data[,23])
    
    final.list <- data[(data[,23]) == m,]
    final.list    
}