get_VoS<-function(label,weight){
  if(length(label)!=length(weight)){
    stop("Not Same Length!")
  }
  #x<-round(x =  runif(n = length(label),min = 1,max = 300),digits = 0)
  #y<-round(x =  runif(n = length(label),min = 1,max = 300),digits = 0)
  x<-seq(from = 1,to = 300,by = 300/length(label))
  y<-seq(from = 1,to = 300,by = 300/length(label))
  r<-x<-round(x =  runif(n = length(label),min = 1,max = 255),digits = 0)
  g<-round(x =  runif(n = length(label),min = 1,max = 255),digits = 0)
  b<-round(x =  runif(n = length(label),min = 1,max = 255),digits = 0)
  df<-data.frame(
    id=1:length(label),
    label=label,
    x=x,
    y=y,
    weight=weight,
    red=r,
    green=g,
    blue=b
  )
  df<-df[sample(x = length(label),replace = F),]
  return (df)
}