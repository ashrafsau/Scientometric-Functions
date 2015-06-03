HIndex<-function(Citation)
{ 
  hindex<-0
  if(length(Citation)>0)
  {
    Citation<-c(Citation,0)   
    ordered.Citation<-sort(Citation,decreasing = T)
    
    for(K in 1: length(ordered.Citation))
    {     
      if(ordered.Citation[K]<K)
        break                   
    }
    hindex<-K-1
  }
  hindex
}
