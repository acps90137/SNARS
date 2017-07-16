build=matrix(,length(MATRIX),5)
Totaldatatable<-function(MATRIX,group,firstdate){ 
##設定日期
  Date0 <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out =length(MATRIX))
  Date <- format(Date0, format = "%Y-%m")

for(i in 1:length(MATRIX)){
    SSort=sort(paste(group),index.return=TRUE) 
    RMATRIX[[i]]=MATRIX[[i]][SSort$ix,] 
    CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix] 
    Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])
    
    density[[i]]<-round(edge_density(Mgfam[[i]], loops = FALSE),4)
    diameter[[i]]<-diameter(Mgfam[[i]])
    mean.distance[[i]]=round(mean_distance(Mgfam[[i]],unconnected = TRUE),2)
    Transitivity[[i]]=round(transitivity(Mgfam[[i]]),4)
    
    build[i,1]=Date[i]
    build[i,2]=round(edge_density(Mgfam[[i]], loops = FALSE),4)
    build[i,3]=diameter(Mgfam[[i]])
    build[i,4]= round(mean_distance(Mgfam[[i]], unconnected = TRUE),2) 
    build[i,5]= round(transitivity(Mgfam[[i]]),4) 
    colnames(build)=c("Date",'Density', 'Diameter', 'Mean.Distance', 'Transitivity')  
  }
  return(build)
}
#build=Totaldatatable(MATRIX,group,firstdate)
