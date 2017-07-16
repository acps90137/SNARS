#####使用者執行檔#####
rm(list=ls(all=TRUE))  #清空所有變數
load("C:/Users/user/Desktop/visualization/quant.RData")
setwd("C:/Users/user/Desktop/visualization")  # set working directory
library(shiny)
library(plotly)
library(ggplot2)
library(igraph)
library(visNetwork)
library(quantmod)
library(dplyr)
library(DT)
library(RColorBrewer)

#請使用者輸入矩陣資料，並命名其為變數MATRIX
MATRIX=W_adj$result.A_hat_0.05
#請使用者輸入含有組別與公司縮寫名稱資料，並命名其為變數gdata
gdata <-read.csv("USA_code.rivise.csv",header=T)
#請使用者輸入初始日期(以月來計)，並命名其為變數firstdate  
firstdate="2006-08-01"
#請使用者選擇三角形數目的門檻觀察值，並命名其為變數obs
obs=50
#----------------------------------------------
RMATRIX=list();CMATRIX=list();deg=list();indeg=list();outdeg=list();datat=list()
Mgfam=list();Mgfam_edge=list();Mgfam_node=list();size=list();assorta=list();as=list();
net=list();net_edge=list();net_node=list();id=list();lable=list()
select.edgein=list();indegposi=list();indegedgelist=list();
indeg_edge=list();indeg_node=list();MAXINdeg=list();indeg0_node=list();indeg0_edge=list()
select.edgeout=list();outdegposi=list();outdegedgelist=list();outdeg_edge=list();
outdeg_node=list();id=list();MAXoutdeg=list();outdeg0_node=list();outdeg0_edge=list()
sunid=list();starid=list()
triangle_number=list();Transitivity=list();density=list()
data=list();diameter=list();nodes.diameter=list();nodes.diameterdf=list();
max.degree=list();neigh.nodes=list();Neighbor.num=list()
mean.distance=list();M=list();dia_edge=list();dia_node=list();nodes.diameter=list()
max.degree=list();neigh.nodes=list();neigh.node.id=list();neigh.edge.posi=list();
neigh.edgelist=list();neigh_edge=list();neigh_node=list();Neighbor.num=list();

#----------------------------------------------------------------#
group=gdata$group            #節點組別
gname =gdata$abbreviation    #節點縮寫名稱
nodenum=dim(MATRIX[[1]])[1]  #網絡節點數
##設定日期
Date0 <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out =length(MATRIX))
Date <- format(Date0, format = "%Y-%m")
#----------------------------------------------------------------#
for(i in 1:length(MATRIX )){
  SSort=sort(paste(group),index.return=TRUE)  
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  
  Mgfam[[i]] <- graph_from_adjacency_matrix(CMATRIX[[i]])
  deg[[i]] <- centralization.degree(Mgfam[[i]])$res
  indeg[[i]] <- degree(Mgfam[[i]], mode="in")                    #in-degree
  outdeg[[i]] <- degree(Mgfam[[i]],mode="out")                   #out-degree
  density[[i]]<-round(edge_density(Mgfam[[i]],loops = FALSE),4)  #density
  Transitivity[[i]]<-round(transitivity(Mgfam[[i]]),4)           #transitivity
  diameter[[i]]<-diameter(Mgfam[[i]])                            #diameter
  nodes.diameter[[i]]<- get.diameter(Mgfam[[i]])                 #diameter path
  g=group[as.vector(nodes.diameter[[i]])]
  g1=matrix( ,1,length(unique(group)));gn=list()
  for(gg in 1:length(unique(group))){g1[gg]<- length(which(g==unique(group)[gg]))}
  g1[is.na( g1)] <- 0   
  max.degree[[i]]<- which(centralization.degree(Mgfam[[i]])$res==max(centralization.degree(Mgfam[[i]])$res))#max degree
  neigh.nodes[[i]] <- neighbors(Mgfam[[i]], V(Mgfam[[i]])[max.degree[[i]]], mode="all") #Neighbor node of maximum degree
  Neighbor.num[[i]] <-length(neigh.nodes[[i]])   
  mean.distance[[i]]=round(mean_distance(Mgfam[[i]], directed = TRUE, unconnected = TRUE),2) #mean.distance
  
  data[[i]]=data.frame(Date[[i]],density[[i]],Transitivity[[i]],mean.distance[[i]],diameter[[i]], g1,Neighbor.num[[i]])
  df <- do.call(rbind.data.frame, data)  
  colnames(df)=c("Date","density","Transitivity","Mean.Distance","diameter",unique(group),"Neighbor.num")
}
###圖中標記字  使用者可更改字(text)與字的位置(x, y) ### 
a1 <- list(
  x = "2008-09" ,
  y = df$density[which(as.character(df$Date)=="2008-09")],
  text = "2008/9 <br> Financial Crisis",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 10,
  ay = -40)

a2 <- list(
  x = "2008-09" ,
  y = df$Mean.Distance[which(as.character(df$Date)=="2008-09")],
  text = "2008/9 <br> Financial Crisis",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 10,
  ay = -40)

a3 <- list(
  x = "2008-09" ,
  y = df$Transitivity[which(as.character(df$Date)=="2008-09")],
  text = "2008/9 <br> Financial Crisis",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 10,
  ay = -40)

a4 <- list(
  x = "2008-09" ,
  y = df$diameter[which(as.character(df$Date)=="2008-09")]+1.5,
  text = "Financial Crisis",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 10,
  ay = -40)

a5 <- list(
  x = "2008-09" ,
  y = df$Neighbor.num[which(as.character(df$Date)=="2008-09")]+0.5,
  text = "2008/9 <br> Financial Crisis",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 10,
  ay = -40)
#=====================================================================#
#======================大於一個矩陣可使用=============================#
#=====================================================================#
source("FUNCTIONALLPlotlY.R")
##範例一
Shiny.ResulT=ALLPlotlY(MATRIX,group,gname,firstdate,obs,
                       ICON.CODEg="f111",ICON.SIZEg=50,
                       ICON.CODE1=c("f005","f185"),ICON.COLOR1=c("yellow","red"),icon.sizeL1=20,
                       ICON.SIZE1=c(50,70),ICON.LABEL1=c("one link company","company"),
                       ICON.CODE2=rep("f19c",length(unique(group))),ICON.COLOR2=unique(ColoR),
                       ICON.LABEL2=unique(group))
Shiny.ResulT
##範例二
Shiny.ResulT=ALLPlotlY(MATRIX,group,gname,firstdate,obs,
                       ICON.CODEg="f111",ICON.SIZEg=50,
                       ICON.CODE1=c("f005","f185"),ICON.COLOR1=c("yellow","red"),icon.sizeL1=17,
                       ICON.SIZE1=c(40,70),ICON.LABEL1=c("one link company","company"),
                       ICON.CODE2=c("f19c","f111","f0c8"),ICON.COLOR2=c("red","blue","purple"),
                       ICON.LABEL2=unique(group))
Shiny.ResulT
#=====================================================================#
#=========================至少一個矩陣即可使用========================#
#=====================================================================#
source("FUNCTIONPlotlYe.R")
##範例一
Shiny.ResulTe=PlotlY1(MATRIX,group,gname,firstdate,
                      ICON.CODEg="f111",ICON.SIZEg=50,
                      ICON.CODE1=c("f005","f185"),ICON.COLOR1=c("yellow","red"),icon.sizeL1=20,
                      ICON.SIZE1=c(50,70),ICON.LABEL1=c("one link company","company"),
                      ICON.CODE2=rep("f19c",length(unique(group))),ICON.COLOR2=unique(ColoR),
                      ICON.LABEL2=unique(group))
Shiny.ResulTe
##範例二
Shiny.ResulTe=PlotlY1(MATRIX,group,gname,firstdate,
                      ICON.CODEg="f111",ICON.SIZEg=50,
                      ICON.CODE1=c("f005","f185"),ICON.COLOR1=c("yellow","red"),icon.sizeL1=20,
                      ICON.SIZE1=c(50,70),ICON.LABEL1=c("one link company","company"),
                      ICON.CODE2=c("f19c","f111","f0c8"),ICON.COLOR2=c("red","blue","purple"),
                      ICON.LABEL2=unique(group))
Shiny.ResulTe