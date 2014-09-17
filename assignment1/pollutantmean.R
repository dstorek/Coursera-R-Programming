# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified
# in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 
# A prototype of the function is as follows

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

    wd <-getwd() # store working directory
    my_vector <- c()
    my_pollutant <- c()
    
    if ( pollutant == "sulfate") {
            my_pollutant <- c(2)
        }
        
    else {
            my_pollutant <- c(3)
    }
    
    for (i in id){  
            if (i < 10) {
            i <- paste("00", i, sep = "")
        }
        
        if (i < 100 & i > 9) {
            i <- paste("0", i, sep = "")
        }
                        
        file_name <- paste(wd, "/",directory, "/", i, ".csv", sep = "")
        # print(file_name)
        my_file <- read.csv(file_name)
        my_vector <- c(my_vector, my_file[, my_pollutant])
    }
    my_vector <- na.omit(my_vector)
    mean(my_vector)
}

#   Output example
#    source("pollutantmean.R")
#    pollutantmean("specdata", "sulfate", 1:10)
#    ## [1] 4.064
#    pollutantmean("specdata", "nitrate", 70:72)
#    ## [1] 1.706
#    pollutantmean("specdata", "nitrate", 23)
#    ## [1] 1.281