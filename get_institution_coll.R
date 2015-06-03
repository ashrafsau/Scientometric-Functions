get_institution_coll<-function(Address, Inst.List=charater(0)){
  #Input: Adrress vector and Institution List
  #Output: Institution collaboration matrix
  
  if(length(Inst.List)<2)stop("Inst.List must be of al least length 2!")
  if(sum(Inst.List=="")>0)stop("Some isntitution's values are empty!")
    
  library(stringr)
  Address<-Address[Address!=""]
  Inst.List<-tolower(Inst.List)
  
  m<-matrix(data=0,nrow=length(Inst.List), ncol = length(Inst.List), dimnames = list(Inst.List,Inst.List))
  
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
    if(length(a)>1){
      rc<-t(combn(x = a,m = 2))
      m[rc]<-m[rc]+1          
    }
   setTxtProgressBar(pb, i)
  }
  m<-m+t(m)
}
