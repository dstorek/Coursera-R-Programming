# Write a function that takes a directory of data files and a threshold for complete cases 
# and calculates the correlation between sulfate and nitrate for monitor locations
# where the number of completely observed cases (on all variables) is greater than the threshold.
# The function should return a vector of correlations for the monitors that meet the threshold requirement.
# If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
# A prototype of this function follows

corr <- function(directory, threshold = 0) {

    
    my_df <- complete(directory, 1:332)
    my_vector <- c()
    
    for ( i in 1:nrow(my_df) ) {
        if ( my_df[i, 2] < threshold) {
            my_vector <- c(my_vector, i)
        }
    }
    
    if ( length(my_vector) > 0 ) {
        my_df <- my_df[-my_vector,]
    }

    if (nrow(my_df) == 0) {
        result <- numeric(length = 0)
        return(result)                    # exit prematurely when there are no monitors above treshold
    }
        
    my_directory <- paste(getwd(), "/", directory, sep = "")

    my_vector <- c() 
    for (i in 1:nrow(my_df)) {
        #print(my_df)
        file_id <- my_df[i, 1]
        #print(file_id)
        if (file_id < 10) {
            file_id <- paste("00", file_id, sep = "")
            }
        if (file_id < 100 & file_id > 9) {
            file_id <- paste("0", file_id, sep = "")
            }   

        my_csv <- read.csv(paste(my_directory, "/", file_id, ".csv", sep = ""))
        my_csv <- na.omit(my_csv)
        my_csv <- my_csv[, 2:3]
        cor <- cor(my_csv)
        my_vector <- c(my_vector, cor[1,2])    
    }
    if ( length(my_vector) < 1) {
        my_vector <- c(0)
    }
        my_vector
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
}

#
#    source("corr.R")
#    source("complete.R")
#    cr <- corr("specdata", 150)
#    head(cr)
#    ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
#    summary(cr)
#    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    ## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
#    cr <- corr("specdata", 400)
#    head(cr)
#    ## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
#    summary(cr)
#    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    ## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
#    cr <- corr("specdata", 5000)
#    summary(cr)
#    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    ## 
#    length(cr)
#    ## [1] 0
#    cr <- corr("specdata")
#    summary(cr)
#    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    ## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
#    length(cr)
#    ## [1] 323