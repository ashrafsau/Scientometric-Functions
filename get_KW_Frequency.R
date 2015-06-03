get_KW_Frequency<-function(kw_set_vector_){
  #Input: Keyword List vectors
  #Output: Keyword List with frequency of occurrence
  library(stringr)
  kw_set_vector_<-kw_set_vector_[kw_set_vector_!=""]
  kw.list<-unlist(str_split(string = kw_set_vector_,pattern = ";"))
  kw.list<-tolower(str_trim(kw.list))
  kw.list<-kw.list[kw.list!=""]
  kw.list<-str_replace(string = kw.list,pattern = "s$","")
  kw.list<-sort(table(kw.list),decreasing = T)
  df<-data.frame(Keyword=names(kw.list),Frequency=kw.list)
}

#df<-get_KW_Frequency(kw_set_vector_ = data$DE)
