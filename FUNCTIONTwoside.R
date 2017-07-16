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
#--------------------------------------------------------------------------------# 
################左邊的點指向右邊的點，代表左邊的點會被右邊的點影響################
#--------------------------------------------------------------------------------# 
visGroup=list()
FUNCTIONTwoside<-function(MATRIX,group,gname,firstdate,ICON.CODEg="f111",ICON.COLOR2=unique(ColoR),ICON.SIZEg=50){
for(i in 1:length(MATRIX) ){
#設定節點圖示的顏色(icon.color)的值
  iconcolor=c()
  for( sg in 1:length(unique(factor(group)))){
    eachg=summary(factor(group))[sg]
    iconcolor = c(iconcolor,rep(ICON.COLOR2[sg],eachg))  
  }
#排序組別，將資料矩陣依照group去做排序
  SSort=sort(paste(group),index.return=TRUE)  
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
#matrix轉成igraph
  Mgfam[[i]]<- graph_from_adjacency_matrix( CMATRIX[[i]]) 
#matrix轉成incidence matrix
  net[[i]] <- graph_from_incidence_matrix( CMATRIX[[i]])          
#節點數  
  n=dim( CMATRIX[[i]])[1]  
#看節點有幾組類別
  ugroup=unique(group)     
  
#調整邊的訊息
  net_edge[[i]]<- data.frame(from = get.edgelist(net[[i]])[,1],to = get.edgelist(net[[i]])[,2])
#調整節點的訊息  
  net_node[[i]]=data.frame(id=1:(2*n), shape = "icon",
                           icon.code =rep(ICON.CODEg,2*n),
                           icon.color =rep(iconcolor,2), 
                           icon.size=rep(ICON.SIZEg,2*n),
                           group=rep(group[SSort$ix],2),
                           #font.size=ICON.SIZEg-6,
                           title = paste0("</b><br>Node </p>", rep(gname[SSort$ix],2),"<p><b>"))
#調整legend的圖示 
  addg <- data.frame(label = as.character(unique(group)), shape = "icon",
                     icon.code =rep(ICON.CODEg,length(ugroup)), icon.size = rep(13,length(ugroup)), icon.color = unique(iconcolor))
  
#調整節點和整個圖的比例位置
  a=matrix(c(seq(-25,465,490/(n-1))),nrow=2*n, ncol =1) #y軸座標
  b=matrix(c(c(rep(0,n),rep(1,n))),nrow=2*n, ncol =1)   #x軸的座標
  ll=matrix(c(b,a),2*n,2)
#網絡圖  
  visGroup[[i]]<-visNetwork(net_node[[i]], net_edge[[i]],
                            main=paste(Date[i]),
                            submain=list(text = c(paste("Node",vcount(Mgfam[[i]]), sep=" = "),
                                       paste("Edge",ecount(Mgfam[[i]]), sep=" = "),
                                       paste("Density",round(edge_density(Mgfam[[i]]),3), sep=" = ")),
                            style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"),
                            footer = "Relation between company",
                height = "500px", width = "100%") %>%
                addFontAwesome() %>%
                visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),arrowStrikethrough=FALSE)  %>%
                visOptions(selectedBy = list(variable = "group",hideColor="rgba(200,200,200,0.65)"), 
                           highlightNearest = list(enabled = TRUE)) %>% 
                visLegend(addNodes = addg,ncol =1,width=0.1, useGroups = FALSE)  %>% 
                visIgraphLayout( layout = "layout.norm",layoutMatrix = ll) 
}
  return(visGroup)
}