
adjlistTograph<-function(df){
  edge.df<-data.frame(stringsAsFactors = F)
  pb <- txtProgressBar(min = 1, max = 100, style = 3)
  for(i in 1: 100){#dim(df)[1]){
    w<-table(str_split(df$Y[i],","))
    xyw<-data.frame(X=df$X[i],w)
    edge.df<-rbind(edge.df,xyw)
    setTxtProgressBar(pb, i)
  }
  edge.df<-edge.df[order(edge.df$Freq,decreasing = T),]  
}
