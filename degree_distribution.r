##########################degree distribution##########################
library(poweRlaw)
library(igraph)
RMATRIX=list();CMATRIX=list();MATRIX=list();Mgfam=list();
tdata=list()
indata=list()
outdata=list()
deg=list()
indeg=list()
outdeg=list()
tm_pl=list()
inm_pl=list()
outm_pl=list()
test_pl=list()
inest_pl=list()
outtest_pl=list()
tpv=list()
inpv=list()
outpv=list()
tbs_p=list()
inbs_p=list()
outbs_p=list()

#請使用者輸入矩陣資料，並命名其為變數MATRIX
MATRIX=W_adj$result.A_hat_0.05
#請使用者輸入含有組別與公司縮寫名稱資料，並命名其為變數gdata
gdata <-read.csv("USA_code.rivise.csv",header=T)
group=gdata$group
gname =gdata$abbreviation
#請使用者輸入初始日期(以月來計)，並命名其為變數firstdate  
firstdate="2006-08-01"
#---------------------------------------------------------------------------
buildtablepl=matrix(,length(MATRIX),6)

for(i in 1:length(MATRIX)){
  SSort=sort(paste(group),index.return=TRUE) 
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])
  
  deg[[i]] <- degree(Mgfam[[i]])
  indeg[[i]] <- degree(Mgfam[[i]],mode="in")
  outdeg[[i]] <- degree(Mgfam[[i]],mode="out")
  
  tdata[[i]]<- deg[[i]][deg[[i]] > 0]
  indata[[i]]<- indeg[[i]][indeg[[i]] > 0]
  outdata[[i]]<- outdeg[[i]][outdeg[[i]] > 0]
  
  tm_pl[[i]] = conpl$new(tdata[[i]])
  inm_pl[[i]] = conpl$new(indata[[i]])
  outm_pl[[i]] = conpl$new(outdata[[i]])
  
  test_pl[[i]] = estimate_xmin(tm_pl[[i]]) # 估計x的下界:"xmin"，和est_pl$pars的值為alpha   #estimate_pars(m_pl[[i]])  #pars的值為alpha
  inest_pl[[i]] = estimate_xmin(inm_pl[[i]])
  outtest_pl[[i]] = estimate_xmin(outm_pl[[i]])

  tm_pl[[i]]$setXmin(test_pl[[i]]) #將上面估出的xmin、alpha 設定到m_pl裡(m_pl中的值就變了)
  inm_pl[[i]]$setXmin(inest_pl[[i]]) 
  outm_pl[[i]]$setXmin(outtest_pl[[i]]) 
#####檢驗資料是否為power law distribution
  tbs_p[[i]] = bootstrap_p(tm_pl[[i]],no_of_sims = 5000, threads = 4, seed = 1)
  tpv[[i]]=tbs_p[[i]]$p  # H0 : data is generated from a power law distribution.    p-value>=0.01 為接受h0
 
  inbs_p[[i]] = bootstrap_p(inm_pl[[i]],no_of_sims = 5000, threads = 4, seed = 1)
  inpv[[i]]=inbs_p[[i]]$p 
  
  outbs_p[[i]] = bootstrap_p(outm_pl[[i]],no_of_sims = 5000, threads = 4, seed = 1)
  outpv[[i]]=outbs_p[[i]]$p 
 
  buildtablepl[i,1]=tbs_p[[i]]$p           #檢驗p-value
  buildtablepl[i,2]=inbs_p[[i]]$p
  buildtablepl[i,3]=outbs_p[[i]]$p
  buildtablepl[i,4]= test_pl[[i]]$pars     #估計total degree的 alpha
  buildtablepl[i,5]= inest_pl[[i]]$pars    #估計in degree的 alpha
  buildtablepl[i,6]= outtest_pl[[i]]$pars  #估計in degree的 alpha
}
colnames(buildtablepl)=c("Total degree's p-value","Indegree's p-value","Outdegree's p-value","Total degree's alpha","Indegree's alpha","Outdegree's alpha")
buildtablepl

####################################################################################
#####deree distribution
setEPS()
postscript("totaldegree.eps")
#par(mfrow=c(1,3))
##deree distribution
for(i in 1:length(MATRIX)){
    SSort=sort(paste(group),index.return=TRUE) 
    RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
    CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
    Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])
    
    plot(degree_distribution(Mgfam[[i]],mode="total"),main=Date[i],type="p",pch=20,xlab="Degree", ylab="Frequency")
    axis(1,labels=c(1:length(degree_distribution(Mgfam[[i]],mode="total"))), at=1:length(degree_distribution(Mgfam[[i]],mode="total")), las=1)
  }
dev.off()
setEPS()
postscript("indegree.eps")
##indegree distribution
for(i in 1:length(MATRIX)){
  SSort=sort(paste(group),index.return=TRUE)  
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])
  
  plot(degree_distribution(Mgfam[[i]],mode="in"),main=Date[i],type="p",pch=20,xlab="InDegree", ylab="Frequency")
  axis(1,labels=c(1:length(degree_distribution(Mgfam[[i]],mode="in"))), at=1:length(degree_distribution(Mgfam[[i]],mode="in")), las=1)
}
dev.off()
setEPS()
postscript("outdegree.eps")
##outdegree distribution
for(i in 1:length(MATRIX)){
  SSort=sort(paste(group),index.return=TRUE) 
  RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
  CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
  Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])
  
  plot(degree_distribution(Mgfam[[i]],mode="out"),main=Date[i],type="p",pch=20,xlab="OutDegree", ylab="Frequency")
  axis(1,labels=c(1:length(degree_distribution(Mgfam[[i]],mode="out"))), at=1:length(degree_distribution(Mgfam[[i]],mode="out")), las=1)
}
dev.off()