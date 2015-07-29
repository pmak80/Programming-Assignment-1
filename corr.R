corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ##  Read data from the directory using the complete function
  ##  created in the previous assignment.
  ##  Total of 332 csv files.
  ##  Put the data into a vector and get the subject of the
  ##  vector that meets the conditions of the parameter values.
  my_data <- complete(directory, 1:332)
  my_vect <- as.vector(my_data)
  meet_id  <- subset(my_vect, nobs > threshold, select = id) 
  
  ## create a list with all 0's 
  correlation <- rep(0, nrow(meet_id))

  ## Read the csv files
  if(nrow(meet_id) > 0){
    
    loc <- paste(getwd(), directory, sep = "/")
    file_list<- list.files(loc)  
  
  ## x is a counter variable and used to read into the correlation 
  ## variable
    x <- 1
    for (i in 1:nrow(meet_id)) {
      id <- meet_id[i, ]
      read_corr <- assign(file_list[i], read.csv(paste(loc, file_list[id], sep="/")))
      clean_corr <- na.omit(read_corr)
      correlation[x] <- cor(clean_corr$nitrate, clean_corr$sulfate)
      x <- x + 1
    }
  }
  correlation
}