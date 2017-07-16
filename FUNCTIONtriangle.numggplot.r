#################################################
##traingle numbers of each node in 36 months##
#################################################
triangle_number=list()
datatr=list()
ptri=list();pt=list()
TriangleNumggplot<-function(MATRIX,group,gname,firstdate){
for(i in 1:length(MATRIX)){
#設定日期 
    Date <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out =length(MATRIX))
    Date <- format(Date, format = "%Y-%m")
#節點數
    nodenum=dim(MATRIX[[1]])[1]  

#排序組別，將資料矩陣依照group去做排序
  rownames(MATRIX[[i]])=colnames(MATRIX[[i]])=1:nodenum
  SSort=sort(paste(group),index.return=TRUE)  
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  Mgfam[[i]] <- graph_from_adjacency_matrix(CMATRIX[[i]])
  
  triangle_number[[i]] <- count_triangles(Mgfam[[i]], vids =V(Mgfam[[i]] )$name)
  if(all(unique(triangle_number[[i]])==0)){
    print("transitivity=0")
  }else{  
  datatr[[i]]=data.frame(Date[i],as.numeric(V(Mgfam[[i]])$name), triangle_number[[i]])
  colnames(datatr[[i]]) <- c("Date","name","Triangle.Numbers")

#ggplot
  pt[[i]] <- ggplot(datatr[[i]],aes(x= name ,y=Triangle.Numbers))+
    geom_bar(aes(fill=Date),stat = "identity",position="dodge",width=0.8)+
    labs(title="<b> Numbers of Triangle about each Node participation",fill="")+
    xlab("Node") + ylab("Triangle Numbers") + 
    facet_grid(.~Date) + 
    theme(legend.position="none",
          axis.text.x  = element_text(angle=35,margin=margin(10,10,10,10,"pt")) 
          ,plot.margin = unit(c(1.5,1.5,2,2),"cm")
          ,legend.title = element_blank()
          ,panel.background = element_rect(fill = "gray") 
          ,panel.grid =element_blank() )
#ggplotly
  ptri[[i]] <- ggplotly(pt[[i]])%>%
              add_annotations( text=paste("Transitivity = ",round(transitivity(Mgfam[[i]]),4)), xref="paper", yref="paper",
                     x=0, xanchor="left",
                     y=1.08, yanchor="middle",   
                     legendtitle=TRUE, showarrow=FALSE ) %>%
              layout(showlegend = FALSE)
  
  }
  }
  return(ptri)
}