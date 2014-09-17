
best <- function(state, outcome) {
    # read outcome data
    outcomes.csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    states.csv <- read.csv("hospital-data.csv")
    states <- levels(factor(states.csv$State))
    
    # check that state and outcome are valid
    if ( !state %in% states) {
        stop("invalid state")
    }
    
    if ( !outcome %in% outcomes) {
        stop("invalid outcome")
    }
      
    # return hospital with lowest 30-day death
    # a <-tapply(outcomes.csv$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcomes.csv$State, mean) 
    # a <-tapply(outcomes.csv[, 11], outcomes.csv[, 7], max) 
    
    outcomes.csv <- outcomes.csv[, c(2,7,11,17,23)]
    names(outcomes.csv)[1] <- "name"
    names(outcomes.csv)[2] <- "state"
    names(outcomes.csv)[3] <- "heart attack"
    names(outcomes.csv)[4] <- "heart failure"
    names(outcomes.csv)[5] <- "pneumonia"
    
    outcomes.csv <- outcomes.csv[outcomes.csv$state==state & outcomes.csv[outcome] != 'Not Available', ]
    values <- outcomes.csv[, outcome]
    rowNum <- which.min(values)
    outcomes.csv[rowNum, ]$name
    # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    # [, 11]
    
    # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    # [, 17]
    
    # Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    # [, 23]
}