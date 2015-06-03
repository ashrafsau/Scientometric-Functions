merge_data<-function(data_dir_,data_source_=c("Scopus","WoS"), year_, DirNos=numeric(0)){
  #Input: data_dir= where data in csv format is stored &
  # year_= target years (eg. 2001-2010)
  #Output: list (Directory info & combined data as a dataframe)
  options(stringsAsFactors = F)
  
  if(length(data_source_)!=1)stop("data_source is not defined!")
  
  library(stringr)
  years<-sort(unique(year_))
  
  dir.list<-list.dirs(path = data_dir_,full.names = T,recursive = F)  
  if(length(dir.list)<1)stop("data_dir_ does not contain data folder!")
  
  if(length(DirNos)>0)dir.list<-dir.list[1:DirNos]
  DirNos<-length(dir.list)
  
  data<-data.frame(stringsAsFactors = F)
  count<-numeric(length = length(dir.list))
  pb <- txtProgressBar(min = 1, max = length(dir.list)+1, style = 3)#Progress Bar
  
  if(data_source_=="WoS"){
    for(d in 1: length(dir.list)){
      csv.list<-list.files(path = dir.list[d],pattern = ".csv$",full.names = T,recursive = T)
      tmp_data<-data.frame(stringsAsFactors = F)
      for(f in csv.list){
        csv.data<-read.csv(file = f)
        if(colnames(csv.data)[1]!="PT"){
          colnames(csv.data)<-colnames(data)
          tmp_data<-rbind(tmp_data,csv.data)
        }
        else tmp_data<-rbind(tmp_data,csv.data)
      }
      
      #refine the data
      py<-as.numeric(tmp_data$PY)
      py[is.na(py)]<-0
      tmp_data<-tmp_data[py %in% years,]
      
      tmp_data$Z9<-as.numeric(tmp_data$Z9)
      tmp_data$Z9[is.na(tmp_data$Z9)]<-0  
      
      count[d]<-dim(tmp_data)[1]
      data<-rbind(data,tmp_data)
      setTxtProgressBar(pb, d)
    }    
  }
  
  if(data_source_=="Scopus"){
    for(d in 1: length(dir.list)){
      csv.list<-list.files(path = dir.list[d],pattern = ".csv$",full.names = T,recursive = T)
      tmp_data<-data.frame(stringsAsFactors = F)
      for(f in csv.list){
        csv.data<-read.csv(file = f)
        tmp_data<-rbind(tmp_data,csv.data)
      }
      
      #refine the data
      py<-as.numeric(tmp_data$Year)
      py[is.na(py)]<-0
      tmp_data<-tmp_data[py %in% years,]
      
      tmp_data$Cited.by<-as.numeric(tmp_data$Cited.by)
      tmp_data$Cited.by[is.na(tmp_data$Cited.by)]<-0  
      
      count[d]<-dim(tmp_data)[1]
      data<-rbind(data,tmp_data)
      setTxtProgressBar(pb, d)
    }    
  }
  
  nm<-list.dirs(path = data_dir_,full.names = F,recursive = F)
  DirectoryInfo<-data.frame(DirectoryName=nm[1:DirNos], Document=count)
  list(DirectoryInfo,data)
}


#df.info<-merge_data(data_dir_ = "P:/RS",data_source_ = "WoS",year_ = 1989:2015)
