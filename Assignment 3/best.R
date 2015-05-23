best <- function(state, outcome) {
  #Loading Data from CSV
  df<- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
  # Outcome Check
  if(outcome!="Pneumonia"&outcome!="Heart Attack"&outcome!="Heart Failure") stop("invalid outcome")
  

  
  str1<-"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from"
  if(outcome=="Pneumonia") str2<-paste(str1,outcome,sep = ".")
  else {
    outcome<-strsplit(outcome,split=" ")
    str2<-paste(str1,outcome[[1]][1],outcome[[1]][2],sep = ".")
  }
  #Removing Rows Having NA VAlues
  good <- complete.cases(df)
  df<-df[good, ][ , ]
  
  df[ ,str2]<-as.numeric(df[ ,str2])
  #State selected
 df1<-df[df$State == state, ]
  #Checking State
 if(nrow(df1)==0) stop("invalid state")
  
  #print(head(df1[ ,1:4]))
  #print(str2)
  df2<-df1[ ,str2]
 
  #print(class(df2))
  #print(head(df2))
  m<-which.min(df2)
  print(min(df2,na.rm=TRUE))
  
  
  df1[m,"Hospital.Name"]
  
  
  }