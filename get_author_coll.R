get_author_coll<-function(author_, author_separator){
  #Input: authors vector and author separator (; or ,)
  #Output: author collaboration matrix
  
  library(stringr)
  no.of.authors<-sapply(X=str_split(author_,author_separator),FUN=length)
  author_<-author_[no.of.authors>1]
  
  au<-str_trim(unlist(str_split(string = author_,pattern = author_separator)))
  au<-sort(unique(tolower(au)))
  
  m<-matrix(data=0,nrow=length(au), ncol = length(au), dimnames = list(au,au))
  
  #pb <- txtProgressBar(min = 1, max = length(author_), style = 3)
  for(i in 1: length(author_)){
    #show(i)
    a<-str_trim(unlist(str_split(string = author_[i],pattern = author_separator)))
    a<-sort(unique(tolower(a)))
    if(length(a)>1){
      rc<-t(combn(x = a,m = 2))
      m[rc]<-m[rc]+1          
    }
    #setTxtProgressBar(pb, i)
  }
  m<-m+t(m)
}
