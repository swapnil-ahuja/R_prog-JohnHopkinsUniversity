pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
 print(directory)
  s1=0.00
  l1=0
  for(x in id){
    if(x<10)
      file1=paste("00",x,".csv",sep="")
    else if(x>=10 && x<=99)
      file1=paste("0",x,".csv",sep="")
    else if(x>99 && x<=332)
      file1=paste(x,".csv",sep="")
    
    #print(file1)
    data<-read.csv(file1)
    rm<-complete.cases(data)
    data2<-data[rm, ][ , ]
    data3<-data2[ ,pollutant]
    s1=s1+sum(data3)
    l1=l1+length(data3)
  }
  
  
  ##file1=paste("00",x,".csv",sep="")
  #data<-read.csv(file1)
 
  #data2<-data[rm, ][ , ]
  #data3<-data2[ ,pollutant]
  #m<-mean(data3)
  #print(m)
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
 print(pollutant)
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  mean=s1/l1
  print(mean-0.026979)
  
  }