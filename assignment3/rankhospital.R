rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomes.csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    states.csv <- read.csv("hospital-data.csv")
    states <- levels(factor(states.csv$State))
    
    ## Check that state and outcome are valid
    if ( !state %in% states) {
        stop("invalid state")
    }
    
    if ( !outcome %in% outcomes) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate    
   
    outcomes.csv <- outcomes.csv[, c(2,7,11,17,23)]
    names(outcomes.csv)[1] <- "name"
    names(outcomes.csv)[2] <- "state"
    names(outcomes.csv)[3] <- "heart attack"
    names(outcomes.csv)[4] <- "heart failure"
    names(outcomes.csv)[5] <- "pneumonia"
    
    outcomes.csv <- outcomes.csv[outcomes.csv$state==state & outcomes.csv[outcome] != 'Not Available', ]
    
    ## Order the data
    outcomes.csv[outcome] <- as.data.frame(sapply(outcomes.csv[outcome], as.numeric))
    outcomes.csv <- outcomes.csv[order(outcomes.csv$name, decreasing = FALSE), ]
    outcomes.csv <- outcomes.csv[order(outcomes.csv[outcome], decreasing = FALSE), ]
    
    ## Process the num argument
    vals <- outcomes.csv[, outcome]
    if( num == "best" ) {
        rowNum <- which.min(vals)
    } else if( num == "worst" ) {
        rowNum <- which.max(vals)
    } else {
        rowNum <- num
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    outcomes.csv[rowNum, ]$name
    
}