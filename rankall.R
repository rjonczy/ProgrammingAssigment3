rankall <- function(outcome, num = 'best') {
        
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
    
    
    # return NA if num is higher than amount of hospitals in a given state
    #if(class(num) == 'numeric') {
    #    if(num > nrow(data[data$State == s,])) {
    #        return(NA)
    #    }            
    #}
    
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
    names(result) <- c('state', 'hospital', 'mortality')
    
    # subset for given state and remove NAa
    result <- subset(result, !is.na(result$mortality))
    
    # sort in ascending order by state, mortality than hospital name
    index <- with(result, order(state, mortality, hospital))
    result <- result[index,]
    
    # split result data.frame by state
    resultList <- split(result, result$state)
    


    # return from sorted data.frames idx element
    r <- lapply( resultList, function(x, num) { 
        
        # set idx as index to be returned
        if(class(num) == 'numeric') {
            idx <- num
        }
        
        if(class(num) == 'character') {
            
            if(num == 'best')            
                idx <- 1
            
            if(num == 'worst') 
                idx <- nrow(x)
        }        
        
        return( x[idx, c(2, 1)] ) 
        }, num )

    # create empty data.frame to hold final results
    res <- data.frame()

    for (i in 1:length(r)) {
        res <- rbind(res, r[[i]])
    }
    
    # return final result
    return(res)  
    
}

