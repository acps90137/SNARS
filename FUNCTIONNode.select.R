#隨著group數目調整顏色
#方法二
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
col<-gg_color_hue(length(unique(group)))
ColoR = col[sort(as.numeric(factor(group)))]
#方法一
#col<-palette(rainbow(length(ugroup)))
#ColoR = col[as.numeric(factor(group))]  
#--------------------------------------------------------------------------------#
##########################使用者可以多重選擇節點的網絡圖##########################
#--------------------------------------------------------------------------------#
visNode_selection=list()  
FUNCTIONNode_selection<-function(MATRIX,group,firstdate,ICON.CODE2=rep("f19c",length(unique(group))),ICON.COLOR2=unique(ColoR)){
#設定節點圖示(icon.code),節點顏色(icon.color)的值
  iconcode=c()
  iconcolor=c()
  for( sg in 1:length(summary(factor(group)))){
    eachg=summary(factor(group))[sg]
    iconcode = c(iconcode,rep(ICON.CODE2[sg],eachg))   
    iconcolor = c(iconcolor,rep(ICON.COLOR2[sg],eachg))  
  }
for(i in 1:length(MATRIX) ){
#排序組別，將數據矩陣依照group去做排序
SSort=sort(paste(group),index.return=TRUE)  
RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
#matrix轉成igraph
Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])         
#計算degree,indegree,outdegree
deg[[i]] <- centralization.degree(Mgfam[[i]])$res 
indeg[[i]] <- degree(Mgfam[[i]], mode="in")
outdeg[[i]] <- degree(Mgfam[[i]],mode="out")

#調整節點訊息
lable[[i]]=id[[i]]=c(1:dim(MATRIX[[i]])[1])
Mgfam_node[[i]]=data.frame(id=id[[i]],
                            label=id[[i]],
                            shape = "icon",
                            icon.code = iconcode,
                            icon.color =iconcolor,
                            icon.size=deg[[i]]+40,
                            deg=deg[[i]],
                            indegree=indeg[[i]],
                            outdegree=outdeg[[i]] ,
                            group=group , font.size=55,
                            title = paste0("Node : ",gname,"<br>Degree : ", deg[[i]]))
#調整邊訊息
Mgfam_edge[[i]] <- get.edgelist(Mgfam[[i]])
Mgfam_edge[[i]]<- data.frame(from =Mgfam_edge[[i]][,1], to = Mgfam_edge[[i]][,2])
#調整legend的圖示
ADDNode <- data.frame(label = as.character(unique(group)), shape = "icon",
                      icon.code = ICON.CODE2, icon.size =17, icon.color =unique(iconcolor))
#網絡圖
visNode_selection[[i]]<-visNetwork(Mgfam_node[[i]],  Mgfam_edge[[i]],
                                   main=paste(Date[i]),
                                   height = "500px", width = "100%") %>%
     visEdges(arrows = 'to',color = list(color = "gainsboro", highlight = "orange"))%>%
     addFontAwesome() %>%
     visLegend(addNodes = ADDNode,ncol =1,width=0.1, useGroups = FALSE) %>%  
     visIgraphLayout(layout ="layout_on_sphere")
}
  return(visNode_selection)
}