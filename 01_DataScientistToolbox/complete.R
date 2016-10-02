complete <- function (directory, id = 1:332) {
    
    #create main data frame
    result.df <- data.frame()
    
    #loop to put all files inside main data frame
    for (f_index in id) {
        
        
        
        #adds maximum No of zeros at the beginning of index e.g. "001"
        zz <- paste("000", f_index, sep="") 
        
        #takes 3 digits from the right to have e.g. "048" instead of "0048"
        f_name <- substr(zz, nchar(zz)- 2, nchar(zz))
        
        
        path <- paste(directory, "/" , f_name , ".csv", sep="")
        
        
        imprtdFile.df <- read.csv(path)
        
        #calculating complete cases
        nobs <- sum(complete.cases(imprtdFile.df))
        
        #creating loop data frame
        loop.df <- data.frame(id = f_index, nobs)
        
        #pasting loop df to result
        result.df <- rbind(result.df, loop.df)
    }
    
    result.df
    
} 
