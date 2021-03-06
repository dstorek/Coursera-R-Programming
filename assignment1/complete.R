# Write a function that reads a directory full of files and reports the number of completely observed
# cases in each data file. The function should return a data frame where the first column 
# is the name of the file and the second column is the number of complete cases. A prototype of this function follows


complete <- function(directory, id = 1:332) {
    
    my_directory <- paste(getwd(), "/", directory, sep = "")
    my_files <- list.files(my_directory)
    my_df <- data.frame(id=integer(), nobs=integer())
    
    for (i in id) {
        my_csv <- read.csv(paste(my_directory, "/", my_files[i], sep = ""))
        current_nobs <- 0
        #print(nrow(my_csv))
        for (b in 1:nrow(my_csv)) {
            # print(my_csv[b,2])
            #if ( my_csv[b, 2] != NA & my_csv[b,3] != NA) {
             if ( !is.na(my_csv[b, 2]) & !is.na(my_csv[b, 3]) ) {
                #print(is.na(my_csv[b, 2]))
                #print(is.na(my_csv[b, 3]))
                current_nobs <- current_nobs + 1
                #print(current_nobs)
            }
        }
        # print(current_nobs)
        new_row <- data.frame(id=my_csv[1,4], nobs=current_nobs)
        my_df <- rbind(my_df, new_row)
    }
    my_df          # returns without printing to console
    #print(my_df)  # returns and prints to console
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
}

#   Example output:
#    source("complete.R")
#    complete("specdata", 1)
#    ##   id nobs
#    ## 1  1  117
#    complete("specdata", c(2, 4, 8, 10, 12))
#    ##   id nobs
#    ## 1  2 1041
#    ## 2  4  474
#    ## 3  8  192
#    ## 4 10  148
#    ## 5 12   96
#    complete("specdata", 30:25)
#    ##   id nobs
#    ## 1 30  932
#    ## 2 29  711
#    ## 3 28  475
#    ## 4 27  338
#    ## 5 26  586
#    ## 6 25  463
#    complete("specdata", 3)
#    ##   id nobs
    ## 1  3  243