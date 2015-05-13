complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  print(directory)
  print(id)
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
    data3<-data[rm, ][ , ]
    #data3<-data2[ ,pollutant]
    l1=0
    l1=l1+nrow(data3)
    
    if(x==min(id))
    df1<-c(x,l1)
    else{
    df2<-c(x,l1)
    df1<-rbind.data.frame(df1,df2)
    }
  }
#   df1<-c(54,219)
   df1<-setNames(df1,c("id","nobs"))
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
#    print(df1)
#    df2<-data.frame(df1)
#    print(class(df2))
#    
#   print("hello")
#   print(df2)
   #df3<-data.frame(id="54",nobs="219")
   #df3
   df1
}