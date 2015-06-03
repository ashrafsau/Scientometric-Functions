GIndex<-function(Citation)
{  
  gindex<-0
  if(length(Citation)>0)
  {
    Citation<-c(Citation,0)
    ordered.Citation<-sort(Citation,decreasing = T)
    s<-0
    for(K in 1: length(ordered.Citation))
    {
      s<-s+ordered.Citation[K]
      if(s<K**2)
        break      
    }
    gindex<-K-1
  }
  gindex
}
