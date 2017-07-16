datatnei=list()
OneinoutF<- function(MATRIX,group,gname,firstdate){
##設定日期  
  Date0 <- seq.Date(from = as.Date(firstdate,format = "%Y-%m-%d"), by = "month", length.out =length(MATRIX))
  Date <- format(Date0, format = "%Y-%m")

##列出一個網絡的date、id、indegree、outdegree的dataframe資料
ID=c(1:dim(MATRIX[[i]])[1])  #節點數
for(i in 1:length(MATRIX)){  
  SSort=sort(paste(group),index.return=TRUE) 
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  
  Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]]) 
  
  indeg[[i]]<-degree(Mgfam[[i]], mode="in")
  outdeg[[i]]<-degree(Mgfam[[i]], mode="out")
  datatnei[[i]]=data_frame(Date[i],ID, indeg[[i]], outdeg[[i]])
  colnames(datatnei[[i]])= c("Date","ID",'InDegree', 'OutDegree')
}
###########################################################
###篩選資料為公司j且含date indegree outdegree的資料，每間公司有36個網絡，所以是36個日期資料
###IDdata裡有71個dataframe ，每個dataframe為一家公司，所以共71家公司，而每家公司有36個日期的資料
IDdata=list()
for(j in 1:length(ID)){
  iddata=c()
  for(i in 1:length(MATRIX)){
    idD=subset(datatnei[[i]],datatnei[[i]]$ID==j, select=c(1,2,3,4))
    idD <- do.call(cbind.data.frame, idD) 
    iddata=rbind(iddata,idD) 
  }
  IDdata[[j]]=iddata  
}
###########################################################
###找一間公司在一個日期(i.e.一個矩陣)之下的indegree(一行>0的公司)、outdegree(一列>0的公司)
###將一間公司在36個時間中的情形，建立的dataframe存入Oneinout，所以Oneinout有71個dataframe，
#  而每個dataframe代表一間公司的to 、from對象有誰
Oneinout=list()

for(k in 1:dim(CMATRIX[[i]])[1]){    #k為第幾間公司
  oneinout=c()
  outnum=c();to=c();from=c();innum=c();inoutid=c();inoutarrow=c();inoutdata=c()
  for(l in 1:length(MATRIX)){        #1~length(MATRIX)間公司的每個公司IN、OUT的狀況
    outnum=which(CMATRIX[[l]][k,]>0) #代表第k公司的outdegree
    to=rep("out",length(outnum))
    innum=which(CMATRIX[[l]][,k]>0)  #代表第k公司的indegree
    from=rep("in",length(innum))
    inoutid=c(outnum,innum)          #合併in、outdegree的公司id
    inoutarrow=c(to,from)            #合併in、outdegree方向的資料
    inoutdata=data.frame(rep(Date[[l]],length(inoutid)),inoutid,inoutarrow)
    colnames(inoutdata)= c("Date","ID",'Arrow')
    oneinout=rbind(oneinout,inoutdata)
  }
  Oneinout[[k]]=oneinout   #將一間公司在所有時間點的情況(時間點共length(MATRIX))，建立dataframe存入Oneinout，所以Oneinout有length(ID)個dataframe
}
return(Oneinout)
}