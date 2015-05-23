rankall <- function(outcome, num = "best") {
  
  df5<- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")  
  st<-df5[,"State"]
  st<-unique(st)
  print(head(st))
  s<-st
    l<-length(s)
   for(count in 1:l){
     if(count==1){
       cf1<-rankhospital(s[count],outcome,num) 
       cf4<-cbind(cf1,s[count])
     }
    else{
      cf1<-rankhospital(s[count],outcome,num)
      cf3<-cbind(cf1,s[count])
      cf4<-rbind(cf4,cf3)
      
    }
      
   }
     
     
    
   
   
  
   
   ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
   colnames(cf4)<-c("hospital","state")
   cf4<-data.frame(cf4)
   cf4<-cf4[order(cf4$state),]
   print((head(cf4)))
   cf4
   }
