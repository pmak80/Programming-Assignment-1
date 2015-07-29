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
  ## NOTE: Do not round the result!
  
    ## Loc variable contains the name if the directory path to 
    ##the csv file
    loc <- paste(getwd(), directory, sep = "/")
    
    ##file_list contains of the csv files in the directory path (loc)
    file_list<- list.files(loc)   
  
    ## Use For loop to read csv files into variable mydata 
    comb_data <- NA
    for (i in id) {
        mydata <- assign(file_list[i], read.csv(paste(loc, file_list[i], sep="/")))
        comb_data <- rbind(comb_data, mydata)
    }

    ##Calculate the Mean on the Pollutant column and remove NA value.
    mean(comb_data[ ,pollutant], na.rm = TRUE)

}

