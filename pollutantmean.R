pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values
  
  poll_means <-vector(mode="numeric")
  test_mean <- 0
  
  for (i in id){
    
    if ((i / 10) < 1){
      filename <- paste("00",i,sep = "")
    }
    else if (i/100 < 1){
      filename <- paste("0",i,sep = "")
    } 
    else {
      filename <-i
    }
    poll_df <- read.csv(file = paste(directory,"/",filename,".csv",sep = ""))
    column <-grep(pollutant, colnames(poll_df))
    
    poll_means <- c(poll_means, poll_df[,column])
  }
  print(mean(poll_means, na.rm = TRUE))
}