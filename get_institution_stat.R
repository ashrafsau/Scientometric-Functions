get_institution_stat<-function(Address,Citation, Inst.List=charater(0)){
  #Input: Adrress vector and Institution List
  #Output: Institution collaboration matrix
  source("P:/Functions/HIndex.R")
  source("P:/Functions/GIndex.R")
  
  if(class(Citation)!="numeric")stop("Citation vector must be numeric!")
  if(length(Inst.List)<2)stop("Inst.List must be of al least length 2!")
  if(sum(Inst.List=="")>0)stop("Some isntitution's values are empty!")
    
  library(stringr)
  Citation<-Citation[Address!=""]
  Address<-Address[Address!=""]
  Inst.List<-tolower(Inst.List)
  
  inst.docs<-character(length = length(Inst.List))
  
  pb <- txtProgressBar(min = 1, max = length(Address), style = 3)
  for(i in 1: length(Address)){
    #
    ins<-str_replace_all(string = Address[i],pattern = "\\[[^\\[]*\\]",replacement = "")
    ins<-str_trim(unlist(str_split(string =ins,pattern = ";")))
    ins<-str_extract(string = ins,pattern = "[a-z A-Z&]+,")
    ins<-str_trim(tolower(str_replace_all(string = ins,pattern = ",",replacement = "")))
    ins<-sort(unique(ins))
    
    #show(i)
    a<-which(Inst.List %in% ins)
    inst.docs[a]<-paste(inst.docs[a],i,sep=",")
    setTxtProgressBar(pb, i)
  }
  
  #TP, TC & Hindex
  dimn<-c("TP","TC","HIndex","GIndex","i10Index","MinCitation","MaxCitation")
  m<-matrix(data = 0,nrow = length(Inst.List),ncol = length(dimn),dimnames = list(Inst.List,dimn))
  
  for(i in 1: length(Inst.List)){
    ind<-as.numeric(unlist(str_split(string = inst.docs[i],pattern = ",")))
    ind<- ind[!is.na(ind)]
    m[i,"TP"]<-length(ind)
    m[i,"TC"]<-sum(Citation[ind],na.rm = T)
    m[i,"HIndex"]<-HIndex(Citation = Citation[ind])
    m[i,"GIndex"]<-GIndex(Citation = Citation[ind])
    m[i,"i10Index"]<-sum(Citation[ind]>=10)
    m[i,"MinCitation"]<-min(Citation[ind],na.rm = T)
    m[i,"MaxCitation"]<-max(Citation[ind],na.rm = T)
  }
  m
}
