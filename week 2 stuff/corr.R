corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        # this uses the cor() function
        
        # make the working directory complete with trailing /
        directory <- paste(directory, "/", sep="") #add trailing /
        
        # get the complete set (using the complete function)
        complete_table <- complete("specdata", 1:332)
        nobs <- complete_table$nobs # 
        
        # Find valid values for comparison
        ids <- complete_table$id[nobs > threshold]
        
        # get length
        id_length <- length(ids)
        
        #create needed vector
        corr_v <- rep(0, id_length)
        
        # fins all the files
        the_files <- as.character(list.files(directory))
        file_paths <- paste(directory, the_files, sep="") #no space, please
        
        #initialize count variable
        count <- 1
        
        # iterate over id
        for(i in ids) {
                current_file <- read.csv(file_paths[i], header=TRUE)
                corr_v[count] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
                count <- count + 1 #increment count
        }
        result <- corr_v
        return(result)
}