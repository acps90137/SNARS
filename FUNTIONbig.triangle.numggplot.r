##################################################
##三角形數目較多的公司(在36個月中)##
##################################################
bigallcdata=data.frame()
library(RColorBrewer)
my_color = c(brewer.pal(n = 9, "Blues"),brewer.pal(n = 9, "Oranges"))
my_color_palette = colorRampPalette(my_color, space = "Lab")
my_color2 = my_color_palette(18)

#obs=50:觀察平均網絡中三角形數>=50的公司
BigtriangleN<-function(MATRIX,group,gname,firstdate,obs=50){
for(i in 1:length(MATRIX)){
#設定日期  
  Date <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out =length(MATRIX))
  Date <- format(Date, format = "%Y-%m")
#節點數
  nodenum=dim(MATRIX[[1]])[1] 
  
  rownames(MATRIX[[i]])=colnames(MATRIX[[i]])=1:nodenum
  SSort=sort(paste(group),index.return=TRUE)  
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  
  Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])  
  
  deg[[i]]<- centralization.degree(Mgfam[[i]])$res
#count_triangles: 計算一個節點有參與幾個三角形
  triangle_number[[i]] <- count_triangles(Mgfam[[i]], vids =V(Mgfam[[i]] )$name)
}

  sumc=c();sumNodedeg=c();onec=c();Nodedegc=c()
  for(j in 1:nodenum){
    onec=c();Nodedegc=c()
    for(i in 1:length(MATRIX)){ 
      
      onec=c(onec,triangle_number[[i]][j])
      Nodedegc=c(Nodedegc,deg[[i]][j])
    }
    sumc=c(sumc,sum(onec))
    sumNodedeg=c(sumNodedeg,sum(Nodedegc))
  }
bigt=which(sumc>=(obs*length(MATRIX)))
if(length(bigt)>0){
Nodedeg=sumNodedeg[bigt] 

bigallc=c()  
for(k in 1:length(bigt)){
  bigonec=c()
  for(i in 1:length(MATRIX)){ 
    bigonec=c(bigonec,triangle_number[[i]][bigt[k]])
  }
  bigallc=data.frame(cbind(bigallc,bigonec))
}
colnames(bigallc)=gname[bigt]
bigallct<-tidyr::gather(bigallc) 
bigallct=cbind(bigallct,NodeDeg=rep(Nodedeg,each=length(MATRIX)))

RRNodedeg=rank(Nodedeg)  

bigallcdata<-cbind(rep(Date,length(bigt)),bigallct,rep(as.factor(RRNodedeg),each =length(MATRIX)))
colnames(bigallcdata)=c("Date","Node","Triangle.Number","NodeDeg","Group")
bigallcdata=bigallcdata[order(bigallcdata$Group),]


bigtr <- ggplot(bigallcdata, mapping = aes(x=Date, y= Triangle.Number,group=Group
                                        ,linetype = factor(bigallcdata$Group,labels =unique(bigallcdata$NodeDeg))
                                        ,colour= factor(bigallcdata$Node)
                                        ,text =paste("Date: ",Date, "<br&gt;Triangle.Number: ",Triangle.Number, "<br&gt;Node:", Node))) +
  geom_line() + geom_point()+ 
  theme(axis.text.x  = element_text(angle=35,margin=margin(10,10,10,10,"pt")) 
        ,plot.margin = unit(c(1,1,2,1),"cm")
        ,legend.title = element_blank()
        ,panel.background = element_rect(fill = "gray20")
        ,panel.grid =element_blank() )+
 scale_colour_manual(values=my_color2[1:length(unique(bigallcdata$Node))])+
ggtitle(label="Big triangle number's node", subtitle =paste0(unique(bigallcdata$Node), collapse = ", "))

bigggplotly<-ggplotly(bigtr , tooltip = c("text"))  %>%  
  add_annotations( text="Node,Degree", xref="paper", yref="paper",
                   x=1, xanchor="left",
                   y=0.8, yanchor="bottom",   
                   legendtitle=TRUE, showarrow=FALSE ) %>%
    layout( legend=list(y=0.8, yanchor="top", font = list(color = "#ffffff"),
            bgcolor = "#000000" ))  
}else{
bigggplotly=print("The number of triangles is too low.")
}
return(bigggplotly)
}