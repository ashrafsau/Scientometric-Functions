get_citation_info<-function(Citation, Years,Target.Years=sort(unique(Years))){
  if(length(Citation)!=length(Years)){
    stop("Citation and Years are not of same length.")
  }
  
  if(!is.numeric(Citation) || !is.numeric(Years)){
    stop("Vectors must be numeric.")
  }
  ind<-which(Years %in% Target.Years)
  
  TC<-aggregate(x = Citation[ind],by = list(Years[ind]),FUN = sum)  
  Cited<-aggregate(x = Citation[ind]>0,by = list(Years[ind]),FUN = sum)
  TP<-as.data.frame(table(Years[ind]))
  
  df<-data.frame(Year=TC$Group.1,TP=TP$Freq,TC=TC$x, Cited=Cited$x)
  return (df)
}