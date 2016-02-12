complete <- function(directory, id=1:332) {
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
        
        # make the working directory complete with trailing /
        directory <- paste(directory, "/", sep="") #add trailing /
        
        # get id vector length
        id_length <- length(id)
        # create vector for cases
        complete_data <- rep(0, id_length)
        
        # find all the files in directory
        the_files <- as.character(list.files(directory))
        file_paths <- paste(directory, the_files, sep="") #no space, please
        
        #initialize count variable
        count <- 1
        
        # iterate over id
        for(i in id) {
                current_file <- read.csv(file_paths[i], header=TRUE)
                complete_data[count] <- sum(complete.cases(current_file))
                count <- count+1 #increment j
        }
        results <- data.frame(id= id, nobs = complete_data)
        return(results)
}