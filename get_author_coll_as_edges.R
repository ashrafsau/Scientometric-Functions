get_author_coll_as_edges<-function(author_, author_separator){
  #Input: authors vector and author separator (; or ,)
  #Output: author adjacency list
  
  library(stringr)
  no.of.authors<-sapply(X=str_split(author_,author_separator),FUN=length)
  author_<-author_[no.of.authors>1]
    
  author.list<-str_trim(unlist(str_split(string = author_,pattern = author_separator)))
  author.list<-sort(unique(tolower(author.list)))
    
  adjacent.list<-character(length(author.list)) #data.frame(stringsAsFactors = F)
  adjacent.list<-NA
  #count<-numeric(0)
  
  show(length(author_))
  pb <- txtProgressBar(min = 1, max = length(author_), style = 3)
  for(i in 1: length(author_)){
    #show(i)
    a<-str_trim(unlist(str_split(string = author_[i],pattern = author_separator)))
    a<-sort(unique(tolower(a)))
    a<-which(author.list %in% a)
    if(length(a)>1){
      rc<-combn(x = a,m = 2)
      v<-aggregate(x = rc[2,],by = list(rc[1,]),FUN = function(x)paste(x,sep="",collapse = ","))
      
      adjacent.list[v$Group.1]<-paste(adjacent.list[v$Group.1],v$x,sep=",")
    }    
    setTxtProgressBar(pb, i)
  }
  adjacent.list<-str_replace(string = adjacent.list,pattern = "NA,",replacement = "")
  ind<-which(adjacent.list!="")
  adjacent.list<-data.frame(X=ind,Y=adjacent.list[ind])
  
  list(author.list,adjacent.list)
}

