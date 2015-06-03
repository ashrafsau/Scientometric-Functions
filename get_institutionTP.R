get_institutionTP<-function(C1_){
  # Extract institution/org name from WoS address field (C1)
  library(stringr)
  C1_<-C1_[C1_!=""]
  C1_<-str_replace_all(string = C1_,pattern = "\\[[^\\[]*\\]",replacement = "")
 
  i.list<-character(0)
  pb <- txtProgressBar(min = 1, max = length(C1_)+1, style = 3)
  for(i in 1: length(C1_)){
    insts<-str_trim(unlist(str_split(string = C1_[i],pattern = ";")))
    insts<-str_extract(string = insts,pattern = "[a-z A-Z&]+,")
    insts<-str_trim(tolower(str_replace_all(string = insts,pattern = ",",replacement = "")))
    i.list<-c(i.list,unique(insts))
    setTxtProgressBar(pb, i)
  }
  
  i.list<-i.list[nchar(i.list)>2]
  institutions_TP<-data.frame()
  if(length(i.list)>0){
    institutions_TP<-as.data.frame(table(i.list),stringsAsFactors = F)
    colnames(institutions_TP)<-c("Institution","Docs")
    institutions_TP<-institutions_TP[order(institutions_TP$Institution),]
  }
  institutions_TP
}

#lis<-get_institutionTP(C1_ = data$C1)
