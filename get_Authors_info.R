get_Authors_info<-function(author_, citation_, author_separator){
  #Input: authors vector and author separator (; or ,)
  #Output: author list with TP & TC
  source("Functions/HIndex.R")
  source("Functions/GIndex.R")
  
  if(length(author_)!=length(citation_))stop("author_ & citation_ is not of same length!")
  
  library(stringr)
  Author.List<-unique(str_trim(unlist(str_split(string = author_,pattern = author_separator))))
  
  dimn<-c("TP","TC","HIndex","GIndex","i10Index","MinCitation","MaxCitation")
  m<-matrix(data = 0,nrow = length(Author.List),ncol = length(dimn),dimnames = list(Author.List,dimn))
  
  for(i in 1: length(Author.List)){
    ind<-which(str_detect(string = author_,pattern = paste(Author.List[i],";|",Author.List[i],"$",sep = "")))
    m[i,"TP"]<-length(ind)
    m[i,"TC"]<-sum(citation_[ind],na.rm = T)
    m[i,"HIndex"]<-HIndex(Citation = citation_[ind])
    m[i,"GIndex"]<-GIndex(Citation = citation_[ind])
    m[i,"i10Index"]<-sum(citation_[ind]>=10)
    m[i,"MinCitation"]<-min(citation_[ind],na.rm = T)
    m[i,"MaxCitation"]<-max(citation_[ind],na.rm = T)
  }
  m
} 
  

#dfr<-get_Top_Authors(author_ = data$AU,citation_ = data$Z9,author_separator = ";")
