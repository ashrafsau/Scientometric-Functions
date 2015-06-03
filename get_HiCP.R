get_HiCP<-function(Citation_,Year_,YearMinCit){
  
  if(mode(Citation_)!="numeric")stop("Citation_ is not numeric!")
  if(class(YearMinCit)!="data.frame")stop("YearMinCit must be a data frame!")
  if(dim(YearMinCit)[2]!=2)stop("YearMinCit should contain two columns namely Year &  MinCitation!")
  if(mode(YearMinCit[,1])!="numeric")stop("First column in YearMinCit is not of numeric type!")
  if(sum(YearMinCit[,1]>1900)!=dim(YearMinCit)[1])stop("Year values are not valid!")
  
  years<-YearMinCit[,1]
  
  m<-matrix(data = 0,nrow = length(years),ncol = 1,dimnames = list(years,"HiCP"))
  
  for(y in years){
    ind<-which(Year_==y)
    thresh<-as.numeric(YearMinCit[YearMinCit[,1]==y,2])
    m[as.character(y),"HiCP"]<-sum(Citation_[ind]>=thresh)
  }
  
  ind<-which(Year_ %in% years)
  TP<-as.data.frame(table(Year_[ind]))  
  df<-data.frame(Year=years,TP=TP$Freq,HiCP=m[,"HiCP"],MinCitation=YearMinCit[,2])
}