complete <- function(directory, id = 1:332) {
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

  nobs <- vector(mode = "numeric")

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
  
  nob<-sum(complete.cases(poll_df))
  #print(nob)
  nobs<-c(nobs,nob)  
}
#print(id)
#print(nobs)
return(data.frame(id = id, nobs = nobs))

}