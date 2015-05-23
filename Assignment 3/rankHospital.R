rankhospital <- function(state, outcome, num = "best") {
  #print(state)
  #print(outcome)
  
  # Outcome Check
  if(outcome!="Pneumonia"&outcome!="Heart Attack"&outcome!="Heart Failure") stop("invalid outcome")
  if(num=="best") num=1
  
  #Loading Data from CSV
  df<- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
  
  str1<-"Hospital.30.Day.Death..Mortality..Rates.from"
  if(outcome=="Pneumonia") str2<-paste(str1,outcome,sep = ".")
  else {
    outcome<-strsplit(outcome,split=" ")
  str2<-paste(str1,outcome[[1]][1],outcome[[1]][2],sep = ".")
  }
  
  df[ ,str2]<-as.numeric(df[ ,str2])
  #state selected
  df<-df[df$State == state, ]
  df1<-df[ ,c("Hospital.Name",str2)]
  df1<-df1[complete.cases(df[ ,str2]), ]#removing NA values
  
  if(num=="worst") num=nrow(df1)
  
  df1<-df1[order(df1$Hospital.Name),]
  rank1<-rank(df1[ ,2],ties.method = "first")#Ranking all the values based on Mortality Rate
  
  
  df2<-(cbind(df1,rank1))
 df2<-df2[order(df2[,"rank1"]),]
 
 if(num<=nrow(df2)) final<-df2[df2$rank1 == num,"Hospital.Name"]
 else final<-NA 
 final
 
}
