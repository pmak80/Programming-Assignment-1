complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV file
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## Loc variable contains the name if the directory path to 
  ##the csv file
  loc <- paste(getwd(), directory, sep = "/")
  
  ##file_list contains of the csv files in the directory path (loc)
  file_list<- list.files(loc)   
  
  
  ## Use For loop to read csv files into variable mydata 
  comb_data <- NA
  for (i in 1:332) {
    mydata <- assign(file_list[i], read.csv(paste(loc, file_list[i], sep="/")))
    comb_data <- rbind(comb_data, mydata)
  }
  
  ## remove all the NA value into variable clean_data
   clean_data <- na.omit(comb_data)
   
   length(id)
   
   ##Return a subset of the record matching the 'id'
   sel_list <- NA
   for(j in 1:length(id)){
      sel_data <- subset(clean_data, ID == id[j] )
      sel_list <- rbind(sel_list, sel_data)
   }
   
  ## The first join contain the first row of record and
  ## in the for loop it will start at row 2 to get the 
  ## record that match the paramaters passed through this function
  join <-cbind(id[1], nrow(subset(sel_list, ID == id[1])))
  if(length(id) > 1){
      
    for(k in 2:length(id)){
          row_cnt <- nrow(subset(sel_list, ID == id[k]))
          join <- rbind(join, cbind(id[k], row_cnt))
    }
  }

  ##name the columns 
  colnames(join) <- c("id", "nobs")
   
  ## change to Data Frame
  result <-as.data.frame(join) 
  result

}
