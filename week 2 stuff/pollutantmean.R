pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        # create a vector to hold requested pollutant data
        pmean_v <- c() #Vector is set to NULL
        
        # find all the files in the folder
        directory <- paste(directory, "/", sep="") #add trailing /
        the_files <- as.character(list.files(directory))
        file_paths <- paste(directory, the_files, sep="") #no space, please
        # iterate over id
        for(i in id) {
                current_file <- read.csv(file_paths[i], header=TRUE)
                head(current_file)
                pollutant
                #remove NA
                removedNA <- current_file[!is.na(current_file[, pollutant]), pollutant]
                # create the mean vector <grr!>
                pmean_v <- c(pmean_v, removedNA)
        }
        #create mean and report
        mean_result <- mean(pmean_v)
        return(round(mean_result, 3)) #3 decimals
}