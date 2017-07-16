#隨著group數目調整顏色
#方法二
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
col<-gg_color_hue(length(unique(group)))
ColoR = col[sort(as.numeric(factor(group)))]
#方法一
# col<-palette(rainbow(length(unique(group))))
# ColoR = col[sort(as.numeric(factor(group)))]
#----------------------------------------------------------------#
########################Diameter路徑網絡圖########################
#----------------------------------------------------------------#
dia.Net=list()
DiameterPatH<- function(MATRIX,group,gname,firstdate,ICON.CODE2=rep("f19c",length(unique(group))),ICON.COLOR2=unique(ColoR)){
#設定節點圖示(icon.code),節點顏色(icon.color)的值
  iconcode=c()
  iconcolor=c()
  for( sg in 1:length(summary(factor(group)))){
    eachg=summary(factor(group))[sg]
    iconcode = c(iconcode,rep(ICON.CODE2[sg],eachg))  
    iconcolor = c(iconcolor,rep(ICON.COLOR2[sg],eachg)) 
  }
#調整legend的圖示
ADDNode <- data.frame(label = as.character(unique(group)), shape = "icon",
                        icon.code = ICON.CODE2, icon.size =rep(20,length(ICON.CODE2)), icon.color =ICON.COLOR2)

for(i in 1:length(MATRIX)){
#排序組別，將資料矩陣依照group去做排序
SSort=sort(paste(group),index.return=TRUE)
RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]

Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]]) 
deg[[i]]<- centralization.degree(Mgfam[[i]])$res 
#標出diameter路徑上的節點
nodes.diameter[[i]]<-get.diameter(Mgfam[[i]])  

#建立edgelist
M[[i]]=matrix(c(nodes.diameter[[i]][-length(nodes.diameter[[i]])],nodes.diameter[[i]][-1]), ncol=2)
#調整邊訊息
dia_edge[[i]]<- data.frame(from = M[[i]][,1],to = M[[i]][,2])
#調整節點訊息
dia_node[[i]]=data.frame(id=as.vector(nodes.diameter[[i]]),
                          label=as.vector(nodes.diameter[[i]]),
                          shape = "icon",
                          icon.code = iconcode[as.vector(nodes.diameter[[i]])],
                          icon.color =iconcolor[as.vector(nodes.diameter[[i]])],
                          icon.size=deg[[i]][as.vector(nodes.diameter[[i]])]+40,
                          group=group[as.vector(nodes.diameter[[i]])],
                          font.size=rep(18,length(nodes.diameter[[i]])),
                          title = paste0("Company : ",gname[as.vector(nodes.diameter[[i]])]
                                         ,"<br> degree= ", degree(Mgfam[[i]])[as.vector(nodes.diameter[[i]])]))
#網絡圖
dia.Net[[i]]<-visNetwork(dia_node[[i]], dia_edge[[i]],
           main=Date[i],
           submain=list(text =paste("Diameter = ",diameter(Mgfam[[i]])),
                        style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"),
           height = "500px", width = "100%") %>%
  addFontAwesome() %>%
  visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 1))
           ,arrowStrikethrough=FALSE)%>%
  visLegend(addNodes = ADDNode,ncol =1,width=0.1, useGroups = FALSE)  %>%
  visIgraphLayout( layout ="layout_in_circle")
}
 return(dia.Net) 
}