co_occuranceMatrix<-function(text_,year_){
  #Input: text vector (eg. title column) & corresponding years
  #Output: matrix (year X words)
  
  library(tm)
  corp<-VCorpus(VectorSource(x = text_))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  
  dtm <- DocumentTermMatrix(corp)
  ord<-order(colSums(as.matrix(dtm)),decreasing = T)
  dtm<-dtm[,ord]
  
  m<-matrix(data=0,nrow = dtm$ncol,ncol = dtm$ncol,
                       dimnames = list(dtm$dimnames$Terms,dtm$dimnames$Terms))
  pb <- txtProgressBar(min = 1, max = dtm$nrow, style = 3)
  for(i in 1: dtm$nrow){
    ind<-which(as.matrix(dtm[i,])>0)
    mn<-t(combn(x = ind,m = 2))
    m[mn]<-m[mn]+1
    setTxtProgressBar(pb, i)
  }
  m
}

wf<-co_occuranceMatrix(text_ = data$TI,year_ = data$PY)
