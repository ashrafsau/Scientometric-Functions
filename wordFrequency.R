wordFrequency<-function(text_,year_){
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
  
  years<-sort(unique(data$PY))
  m<-matrix(data = 0,nrow = length(years),
                          ncol = dtm$ncol,dimnames = list(years,dtm$dimnames$Terms))
  
  for(i in 1: length(years)){
    tdtm<-dtm[data$PY %in% years[i],]
    m[i,]<-colSums(x = as.matrix(tdtm))
  }
  word.freq  
}

wf<-wordFrequency(text_ = data$TI,year_ = data$PY)
