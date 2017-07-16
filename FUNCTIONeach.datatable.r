dataeach=list()
datateach<-function(MATRIX,group,gname,firstdate){
  for(i in 1:length(MATRIX)){
    Date <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out = length(MATRIX))
    Date <- format(Date, format = "%Y-%m")

    SSort=sort(paste(group),index.return=TRUE)
    RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
    CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
    
    Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])  
    
    deg[[i]]<- centralization.degree(Mgfam[[i]])$res 
    indeg[[i]] <- degree(Mgfam[[i]], mode="in")
    outdeg[[i]] <- degree(  Mgfam[[i]],mode="out")
    
    dataeach[[i]]=data_frame(Date[i],gname,group, deg[[i]], indeg[[i]],  outdeg[[i]])
    colnames(dataeach[[i]])= c("Date","Name",'Group', 'Degree', 'InDegree', 'OutDegree')
    
  }
  return(dataeach)
}