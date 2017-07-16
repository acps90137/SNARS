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
#--------------------------------------------------------------------------------------#
#########挑選出所有網絡中最大degree的那些節點，畫出折線圖與它們的一步鄰居網絡圖#########
#--------------------------------------------------------------------------------------#
visNeighbor=list()
NeighborhooD<- function(MATRIX,group,gname,ICON.CODE2=c("f19c","f111","f0c8"),ICON.COLOR2=unique(ColoR),firstdate){
#設定日期
  Date <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out = length(MATRIX))
  Date <- format(Date, format = "%Y-%m")
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
#計算degree，挑出網絡中最大的degree節點
    deg[[i]]<- centralization.degree(Mgfam[[i]])$res 
    max.degree[[i]] <- which(deg[[i]]==max(deg[[i]]))
  
    neigh.nodes0=c();aa=c()
    for(maxd in 1:length( max.degree[[i]])){
      neigh.nodes0[[maxd]]<- as.vector(neighbors(Mgfam[[i]], as.vector(V(Mgfam[[i]])[max.degree[[i]]])[maxd], mode="all"))
      aa=c(aa,neigh.nodes0[[maxd]])
      neigh.nodes[[i]]=unique(aa)  #一個網絡中最大DEGREE的那些節點的鄰居ID
    }
    #鄰居id、鄰居edge位置，建立edgelist
    neigh.node.id[[i]] <- unique(c(as.vector(neigh.nodes[[i]]),  max.degree[[i]]) ) 
    neigh.edge.posi[[i]]=c(which(get.edgelist(Mgfam[[i]])[,2] %in% max.degree[[i]]),which(get.edgelist(Mgfam[[i]])[,1] %in% max.degree[[i]]))
    neigh.edgelist[[i]] <- get.edgelist(Mgfam[[i]])[neigh.edge.posi[[i]],] 
#調整邊訊息    
    neigh_edge[[i]]<- data.frame(from =neigh.edgelist[[i]][,1],to = neigh.edgelist[[i]][,2] )
#調整節點訊息   
    neigh_node[[i]]=data.frame(id= neigh.node.id[[i]],
                               label=  neigh.node.id[[i]],
                               group= group[neigh.node.id[[i]]] ,
                               title = paste0("Node: ",gname[neigh.node.id[[i]]]),
                               shape = "icon",
                               icon.code = iconcode[  neigh.node.id[[i]]],
                               icon.color = iconcolor[neigh.node.id[[i]]],
                               icon.size=deg[[i]][  neigh.node.id[[i]]]+50,
                               font.size=40)
#網絡圖
    visNeighbor[[i]]=visNetwork(neigh_node[[i]],neigh_edge[[i]],
                                main = list(text = Date[i],
                                            style = "font-family:serif;color:#000000;font-size:25px;font-weight:bold;text-align:center;"),
                                submain=  list(text = paste("Max Degree: ", paste(gname[max.degree[[i]]],collapse = ",")
                                                            , "<br>Total Neighbor Numbers: ", unique(deg[[i]][max.degree[[i]]])),
                                               style = "font-family:Comic Sans MS;color:#ff0000;font-size:18px;text-align:center;"),    
                                width = "110%",height="90%") %>%
      addFontAwesome() %>%
      addIonicons()%>%
      visIgraphLayout(layout = "layout_with_drl",physics = TRUE, smooth = T,randomSeed = 123)%>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 1))) %>%   
      visOptions( highlightNearest = list(enabled = TRUE, hover = F, hideColor = 'rgba(200,200,200,0)')  ) %>% 
      visPhysics(solver = "repulsion")%>%
      visInteraction(keyboard = TRUE,dragNodes = T, dragView = T, zoomView = T ) %>%
      visLegend(addNodes = ADDNode, useGroups = FALSE) %>% 
      visClusteringByGroup(groups = as.character( unique(group)), label = c("Group : "), 
                           shape = "icon", color = "blue", force = TRUE) 
  }
  return(visNeighbor) 
}