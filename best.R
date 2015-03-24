best <- function(state, outcome) {
 
    # 1.) Read outcome data
    # lowest 30-day mortality rate
    #    2 - hospital name
    #    7 - state
    #   11 - heart attack
    #   17 - heart failure
    #   23 - preumonia
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
       
    # check if passed outcome param is valid
    if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
        stop("invalid outcome")
    
    # check if passed state is valid        
    if(!state %in% unique(data[,7]))
        stop("invalid state")   
    
    # convert to numeric columns: 11, 17, 23
    data[,11] <- as.numeric(data[,11])
    data[,17] <- as.numeric(data[,17])
    data[,23] <- as.numeric(data[,23])
    
    # convert to factor column: 7
    data[,7] <- as.factor(data[,7])
    
    # subtack columns: 7 - state, 2 - hospital name, 11, 17, 23 - mortality 'heart attack', 'heart failure', 'pneumonia' accordingly
    if(outcome == 'heart attack')
        result <- data[, c(7, 2, 11)]
    
    if(outcome == 'heart failure')
        result <- data[, c(7, 2, 17)]
    
    if(outcome == 'pneumonia')
        result <- data[, c(7, 2, 23)]
    
    # assign column names
    names(result) <- c('state', 'hname', 'mortality')
    
    # subset for given state and remove NAa
    #result <- subset(result, !is.na(result$mortality) & result$state == state)
    s <- state
    result <- subset(result, result$state == s & !is.na(result$mortality))
    
    # sort in ascending order by mortality than hospital name
    index <- with(result, order(mortality, hname))
    result <- result[index,]
    # return
    result[1,'hname']   
    
}