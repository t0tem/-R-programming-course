corr <- function (directory, threshold = 0) {
    
    #initiate result vector
    result.v <- numeric()
    
    
    for ( i in 1:332 ) {
        
        if (complete("specdata", i)[,2] > threshold) {   #uses 'complete' func! 
            
            #piece of code from 'complete' func to import file
            zz <- paste("000", i, sep="")
            f_name <- substr(zz, nchar(zz)- 2, nchar(zz))
            path <- paste(directory, "/" , f_name , ".csv", sep="")
            imprtdFile.df <- read.csv(path)
            
            #subsetting on complete cases
            loop.df <- imprtdFile.df[complete.cases(imprtdFile.df),]
            
            #getting correlation
            loop.cor <- cor(loop.df$sulfate, loop.df$nitrate)
            
            #appending to result
            result.v <- c(result.v, loop.cor)
        }
        
    }
    
    result.v
    
}
