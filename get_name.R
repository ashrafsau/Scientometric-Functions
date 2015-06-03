
get_name<-function(num,max.num){
  # used for file naming when it shoul be of same length
  if(num>max.num){
    stop("num is greater than max.num!!")   
  }
  n<-nchar(max.num)-nchar(num)
  str<-num
  if(n>0)for(i in 1: n)str<-paste("0",str,sep = "")
  str
}