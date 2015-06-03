get_author_pattern<-function(author_, year_, author_separator){
  #Input: authors vector and corresponding year values
  #Output: year-wise authorship
  
  if(length(author_)!=length(year_))stop("author_ and year_ are not of same length!")
  if(mode(year_)!="numeric")stop("year_ must be numeric!")
  # load library
  library(stringr)  
  no.of.authors<-sapply(X=str_split(author_,author_separator),FUN=length)
  max.authors<-max(no.of.authors)
  
  years<-sort(unique(year_))
  m<-matrix(data=0,nrow=length(years), ncol = max.authors, dimnames = list(years,1:max.authors))
  
  for(i in 1: length(years)){
    show(years[i])
    ind<-which(year_==years[i])
    m[i,]<-table(c(no.of.authors[ind],1:max.authors))-1
  }
  
  m<-m[,colSums(m)!=0]
}

#df<-get_author_pattern(author_ = data$AU, year_ = data$PY,author_separator=";")
