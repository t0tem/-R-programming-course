pollutantmean <- function (directory, pollutant, id = 1:332) {
    
    #create main data frame
    main.df <- data.frame()
    
    #loop to put all files inside main data frame
    for (f_index in id) {
        
        #adds maximum No of zeros at the beginning of index e.g. "001"
        zz <- paste("000", f_index, sep="") 
        
        #takes 3 digits from the right to have e.g. "048" instead of "0048"
        f_name <- substr(zz, nchar(zz)- 2, nchar(zz))
        
        path <- paste(directory, "/" , f_name , ".csv", sep="")
        
        loop.df <- read.csv(path)
        
        main.df <- rbind(main.df, loop.df)
    }
    
    mean(main.df[ ,pollutant], na.rm = T)
    # invisible(main.df)
    
} 


