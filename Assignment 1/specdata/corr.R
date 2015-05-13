corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  id<-1:332
  cr<-c()
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
    l1=nrow(data3)
     if(l1>threshold)
       {
       cr1<-cor(data3[ ,"sulfate"],data3[ ,"nitrate"])
       cr<-append(cr,cr1)
               
       }
      
      
  }
    
    
 
  
  
 # if(sul==0)
   # cr<-vector(mode="numeric", length=0)
   
  if(is.null(cr))
    cr<-vector(mode="numeric", length=0)
  cr
}