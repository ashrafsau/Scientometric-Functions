get_HiCP_Limit<-function(Citation_,Year_,TopPercent=5){
  
  if(mode(Citation_)!="numeric")stop("Citation_ is not numeric!")
  
  years<-sort(unique(Year_))
  
  lm<-matrix(data = 0,nrow = length(years),ncol = 1,dimnames = list(years,"MinCit"))
  
  for(y in years){
    ind<-which(Year_==y)
    ct<-sort(Citation_[ind],decreasing = T)
    t<-ceiling(length(ind)*(TopPercent/100))
    lm[as.character(y),"MinCit"]<-ct[t]
  }
  df<-data.frame(Year=as.numeric(dimnames(lm)[[1]]),MinCitation=lm[,"MinCit"])
}