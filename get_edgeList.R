get_edgeList<-function(m){
  library(stringr)
  edge.list<-character(0)
  n<-m*upper.tri(m)
  nodes<-dimnames(n)[[1]]
  nodes<-str_replace_all(string = nodes,pattern = "United States",replacement = "USA")
  nodes<-str_replace_all(string = nodes,pattern = "United Kingdom",replacement = "UK")
  
  w<-numeric(0)
  for(i in 1:nrow(n)){
    j<-which(n[i,]>0)
    #j<-j[j>i]
    if(length(j)>0){
      edge.list<-c(edge.list,paste(nodes[i],nodes[j],n[i,j],sep=","))
      w<-c(w,n[i,j])
    }
  }
  edge.list<-edge.list[order(w,decreasing = T)]
}
