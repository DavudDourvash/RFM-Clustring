##########################################
############# server.R ###################
##########################################

# options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
#### list calc ####
####packages and libraries ####
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("cluster")
# install.packages("lubridate")
# install.packages("openxlsx")
# install.packages("psych")
library(shiny)
library(cluster)
library(lubridate)
require(openxlsx)
library(shinydashboard)
library(psych)
library(ggplot2)
library(radarchart)
library(dygraphs)
####lists####
NewtableData<-list()
countil<-list()
RFM_tablew<-list()
kmw<-list()
wi<-list()
clusterfreqw<-list()
CRFMw<-list()
c1centerww<-list()
cidclustwwi<-list()
cidrfmclustwwi<-list()
distwic<-list()
ciddistclwi<-list()
cidv<-list()
ciddistwi<-list()
distallclww<-list()
#### start functions ####
# How this chunk take "table" and "score" as inputs?
# ordinalScore<-function(table,score){
#   table$R=as.integer(6)-cut(table$R,score,labels = FALSE)
#   table$f=cut(table$f,score,labels = FALSE,include.lowest=TRUE)
#   table$M=cut(table$M,score,labels = FALSE,include.lowest=TRUE)
#   table$L=cut(table$L,score,labels = FALSE,include.lowest=TRUE)
#   return (table)
# }
quantileScore<- function(vec, score){
  scorevec<-vector()
  qu<-quantile(vec, probs = seq(0, 1, 1/score))
  scorevec[which((vec<=qu[[2]]) & (vec>=qu[[1]]))]=1
  for(i in 2:score-1){
    scorevec[which((vec<=qu[[i+1]]) & (vec>qu[[i]]))]=i
  }
  scorevec[which((vec > qu[[score]]))]=score
  scorevec
}

ordinalScore<-function(table,score){
  table$R=as.integer(6)-quantileScore(table$R, score)
  table$f=quantileScore(table$f, score)
  table$M=quantileScore(table$M, score)
  # table$L=quantileScore(table$L, score)
  return (table)
}

# i don't understand Logic og this code chunck
newScore<-function(RFM_table,score){
  minTmp1<-min(RFM_table$f[which(RFM_table$f!=1)])
  RFM_table$f[which(RFM_table$f[which(RFM_table$f<=minTmp1)] >1)]=2
  minTmp1<-min(RFM_table$f[which(RFM_table$f>minTmp1)])
  RFM_table$f[which(RFM_table$f[which(RFM_table$f<=minTmp1)] >2)]=3
  minTmp1<-min(RFM_table$f[which(RFM_table$f>minTmp1)])
  RFM_table$f[which(RFM_table$f[which(RFM_table$f<=minTmp1)] >3)]=4
  RFM_table$f[which(RFM_table$f>minTmp1)]=5
  RFM_table$R[which(RFM_table$R<=60)]=5
  RFM_table$R[which(RFM_table$R[which(RFM_table$R<=120)] >60)]=4
  RFM_table$R[which(RFM_table$R[which(RFM_table$R<=180)] >120)]=3
  RFM_table$R[which(RFM_table$R[which(RFM_table$R<=240)] >180)]=2
  RFM_table$R[which(RFM_table$R>240)]=1
  RFM_table$M=cut(RFM_table$M,score,labels = FALSE)
  RFM_table$L=cut(RFM_table$L,score,labels = FALSE)
  return(RFM_table)
  
}

# it's ok
createRFMTable<-function(tableData){
  R_table<-aggregate(tableData$Date.Miladi ~ tableData$CID,tableData,FUN=max)
  names(R_table) <- c("CId", "R")
  #   L_table<-aggregate(tableData$Date.Miladi ~ tableData$CID,tableData,FUN=min)
  #   names(L_table) <- c("CId", "L")
  maxDate<-max(tableData$Date.Miladi);
  R_table$R <-as.numeric(maxDate - R_table$R[])
  #   L_table$L <-as.numeric(maxDate - L_table$L[])
  F_table <- aggregate(tableData$FactorID ~ tableData$CID, tableData, FUN=length)
  names(F_table) <- c("CId", "f")
  # tableData$total_money<-tableData$unitprice * tableData$value;
  M_table <- aggregate(tableData$Value ~ tableData$CID, tableData,FUN = "sum")
  names(M_table) <- c("CId", "M")
  RFM_table <- merge(R_table,F_table)
  RFM_table <- merge(RFM_table,M_table)
  #   RFML_table <-merge(RFML_table,L_table)
  return (RFM_table)
}

# it's ok
findBestKMeans<-function(RFM_table){
  
  dissE <-daisy(RFM_table[,2:ncol(RFM_table)])
  dE2<- dissE^2
  minSil<-numeric(10)
  maxSeed<-numeric(10)
  sil <- numeric(5)
  for (i in 1:5){
    set.seed(i)
    km <- kmeans(RFM_table[,2:ncol(RFM_table)],centers =  4)
    sil[[i]]<-mean(silhouette(km$cl, dissE)[,3])
  }
  minSil[[4]]<-min(sil)
  maxSeed[[4]]<-which.max(sil)
  kSeed<-maxSeed[4]
  set.seed(kSeed)
  return(kmeans(RFM_table[,2:ncol(RFM_table)],centers =  4))
}

# it seems return of this function equals km$centers, maybe it must remove
findCenters<-function(km){
  
  sizeX<-dim(km$centers)[1]
  sizeY<-dim(km$centers)[2]
  centrs=matrix(list(), nrow=sizeX, ncol=sizeY)
  for(l in 1:sizeX){
    centrs[l,]<-km$centers[l,]
  }
  return(centrs)
}
#### read table from user####
#   inFile <- input$dataFile
#   if (is.null(inFile))
#     return(NULL)
#   if(grepl(".xlsx",inFile$name))
#     tableData<-read.xlsx(inFile$datapath,sheet = 1, startRow =1, colNames = TRUE)
#   else if(grepl(".csv",inFile$name))
#     tableData<-read.csv(inFile$datapath,header=TRUE)
####from data wearhouse ####
#   MData<-reactive ({
#     
#     channel<-odbcConnect("Rsql2")## an DSN calls "hamkaran" made in odbcConnection to sql server
#     MData2<- sqlFetch(channel,sqtable = "clustering")## a table calles hkshamsi made in sql server
#     close(channel)
#     MData2
#   })
#   
#   
#   
#   set.seed(122)
#   tableData <- reactive({
#     if (is.null(MData())) {
#       return(NULL)
#     } else {
#       MData()
#     }
#   })


a<-"H:\\since95\\Dayche-2\\Dropbox\\Dayche-Project-MrDourvash\\hamkaran system\\clustering\\HK-Temp\\data\\ShinyDemo.xlsx"
b<-read.xlsx(a)
tableData<-b
#### make scored RFM table for modeling data ####
tableData$Date.Miladi<-convertToDate(Date.Miladi<-tableData$`Date.Miladi`,origin = "1900-01-01")
tableData<-tableData[order(tableData$Date.Miladi),]
tableDataweek<-as.numeric( format(tableData$Date.Miladi+3, "%U"))
tableData$week <- tableDataweek
Nweeks<-length(unique(tableDataweek))
seventypercweek<-unique(tableDataweek)[ceiling(0.7*Nweeks)]
seventyPerc<-length(tableDataweek[tableDataweek<=seventypercweek])
tableDataOrg<-tableData[1:seventyPerc,]
tableDataNew<-tableData[(seventyPerc+1):nrow(tableData),]
RFM_tablea<-createRFMTable(tableDataOrg)
RFM_table<- ordinalScore(RFM_tablea,5)

#### distribution of table 
# 
# par(mfrow=c(2,3))
# barplot(table(RFM_tablea$R))
# barplot(table(RFM_tablea$f))
# barplot(table(RFM_tablea$M))
# 
# barplot(table(RFM_table$R))
# barplot(table(RFM_table$f))
# barplot(table(RFM_table$M))
# par(mfrow=c(1,1))
#### detect distance matrix for mideling data ####

### clustering modeling ###

km<-findBestKMeans(RFM_table)#k means for w0
w0<-findCenters(km)# centers of k means w0

RFM_table<-cbind(RFM_table, km$cluster)# add cluster of any CID to RFM_table
RFM_tablea<-cbind(RFM_tablea, km$cluster)
clusterfreq<-c(length(km$cl[km$cl==4]),length(km$cl[km$cl==3]), length(km$cl[km$cl==1]),length(km$cl[km$cl==2]))#frequency of every 5 clusters

c1RFM<-RFM_table[km$cl==4,] ;c2RFM<-RFM_table[km$cl==3,] ;c3RFM<-RFM_table[km$cl==1,] ;c4RFM<-RFM_table[km$cl==2,]#divide RFM to clusters

c1center<-km$centers[4,] ; c2center<-km$centers[3,] ; c3center<-km$centers[1,] ; c4center<-km$centers[2,]#fine centers of clusters

CidClustw0<-cbind(RFM_table$CId, km$cluster)#create data frame contain CID and cluster

cidclust1w0<-CidClustw0[CidClustw0[,2] ==4,] ; cidrfmclust1w0<-RFM_table[km$cluster ==4, ]#divide cid as clusters and rfm as clusters
cidclust2w0<-CidClustw0[CidClustw0[,2] ==3,] ; cidrfmclust2w0<-RFM_table[km$cluster ==3, ]
cidclust3w0<-CidClustw0[CidClustw0[,2] ==1,] ; cidrfmclust3w0<-RFM_table[km$cluster ==1, ]
cidclust4w0<-CidClustw0[CidClustw0[,2] ==2,] ; cidrfmclust4w0<-RFM_table[km$cluster ==2, ]

distw0c1<-vector() ; distw0c2<-vector() ; distw0c3<-vector() ; distw0c4<-vector()

for(i in 1:clusterfreq[1]){
  distw0c1[i]<-sqrt((cidrfmclust1w0[i, "R"] - c1center[[1]])^2 + 
                      (cidrfmclust1w0[i, "f"] - c1center[[2]])^2 + 
                      (cidrfmclust1w0[i, "M"] - c1center[[3]])^2)}

for(i in 1:clusterfreq[2]){
  distw0c2[i]<-sqrt((cidrfmclust2w0[i, "R"] - c2center[[1]])^2 +
                      (cidrfmclust2w0[i, "f"] - c2center[[2]])^2 + 
                      (cidrfmclust2w0[i, "M"] - c2center[[3]])^2)}

for(i in 1:clusterfreq[3]){
  distw0c3[i]<-sqrt((cidrfmclust3w0[i, "R"] - c3center[[1]])^2 + 
                      (cidrfmclust3w0[i, "f"] - c3center[[2]])^2 + 
                      (cidrfmclust3w0[i, "M"] - c3center[[3]])^2)}

for(i in 1:clusterfreq[4]){
  distw0c4[i]<-sqrt((cidrfmclust4w0[i, "R"] - c4center[[1]])^2 + 
                      (cidrfmclust4w0[i, "f"] - c4center[[2]])^2 + 
                      (cidrfmclust4w0[i, "M"] - c4center[[3]])^2)}
##### create NewTableData for moving window####
#### NewTableData[[1]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[2 : 33]){
  counti[[k]]<-week
  k<-k+1
}
countil[[1]]<-counti  
NewtableData[[1]]<-tableData[tableData$week %in% countil[[1]],2:ncol(tableData)]

#### NewTableData[[2]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[3 : 34]){
  counti[[k]]<-week
  k<-k+1
}
countil[[2]]<-counti  
NewtableData[[2]]<-tableData[tableData$week %in% countil[[2]],2:ncol(tableData)]

#### NewTableData[[3]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[4 : 35]){
  counti[[k]]<-week
  k<-k+1
}
countil[[3]]<-counti  
NewtableData[[3]]<-tableData[tableData$week %in% countil[[3]],2:ncol(tableData)]

#### NewTableData[[4]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[5 : 36]){
  counti[[k]]<-week
  k<-k+1
}
countil[[4]]<-counti  
NewtableData[[4]]<-tableData[tableData$week %in% countil[[4]],2:ncol(tableData)]

#### NewTableData[[5]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[6 : 37]){
  counti[[k]]<-week
  k<-k+1
}
countil[[5]]<-counti  
NewtableData[[5]]<-tableData[tableData$week %in% countil[[5]],2:ncol(tableData)]

#### NewTableData[[6]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[7 : 38]){
  counti[[k]]<-week
  k<-k+1
}
countil[[6]]<-counti  
NewtableData[[6]]<-tableData[tableData$week %in% countil[[6]],2:ncol(tableData)]

#### NewTableData[[7]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[8 : 39]){
  counti[[k]]<-week
  k<-k+1
}
countil[[7]]<-counti  
NewtableData[[7]]<-tableData[tableData$week %in% countil[[7]],2:ncol(tableData)]

#### NewTableData[[8]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[9 : 40]){
  counti[[k]]<-week
  k<-k+1
}
countil[[8]]<-counti  
NewtableData[[8]]<-tableData[tableData$week %in% countil[[8]],2:ncol(tableData)]

#### NewTableData[[9]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[10 : 41]){
  counti[[k]]<-week
  k<-k+1
}
countil[[9]]<-counti  
NewtableData[[9]]<-tableData[tableData$week %in% countil[[9]],2:ncol(tableData)]

#### NewTableData[[10]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[11 : 42]){
  counti[[k]]<-week
  k<-k+1
}
countil[[10]]<-counti  
NewtableData[[10]]<-tableData[tableData$week %in% countil[[10]],2:ncol(tableData)]

#### NewTableData[[11]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[12 : 43]){
  counti[[k]]<-week
  k<-k+1
}
countil[[11]]<-counti  
NewtableData[[11]]<-tableData[tableData$week %in% countil[[11]],2:ncol(tableData)]

#### NewTableData[[12]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[13 : 44]){
  counti[[k]]<-week
  k<-k+1
}
countil[[12]]<-counti  
NewtableData[[12]]<-tableData[tableData$week %in% countil[[12]],2:ncol(tableData)]

#### NewTableData[[13]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[14 : 45]){
  counti[[k]]<-week
  k<-k+1
}
countil[[13]]<-counti  
NewtableData[[13]]<-tableData[tableData$week %in% countil[[13]],2:ncol(tableData)]
####Create New table data Moving and  timingwindow for 13 weeks####
##### create NewTableData for moving window####
#### NewTableData[[1]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[2 : 33]){
  counti[[k]]<-week
  k<-k+1
}
countil[[1]]<-counti  
NewtableData[[1]]<-tableData[tableData$week %in% countil[[1]],2:ncol(tableData)]

#### NewTableData[[2]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[3 : 34]){
  counti[[k]]<-week
  k<-k+1
}
countil[[2]]<-counti  
NewtableData[[2]]<-tableData[tableData$week %in% countil[[2]],2:ncol(tableData)]

#### NewTableData[[3]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[4 : 35]){
  counti[[k]]<-week
  k<-k+1
}
countil[[3]]<-counti  
NewtableData[[3]]<-tableData[tableData$week %in% countil[[3]],2:ncol(tableData)]

#### NewTableData[[4]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[5 : 36]){
  counti[[k]]<-week
  k<-k+1
}
countil[[4]]<-counti  
NewtableData[[4]]<-tableData[tableData$week %in% countil[[4]],2:ncol(tableData)]

#### NewTableData[[5]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[6 : 37]){
  counti[[k]]<-week
  k<-k+1
}
countil[[5]]<-counti  
NewtableData[[5]]<-tableData[tableData$week %in% countil[[5]],2:ncol(tableData)]

#### NewTableData[[6]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[7 : 38]){
  counti[[k]]<-week
  k<-k+1
}
countil[[6]]<-counti  
NewtableData[[6]]<-tableData[tableData$week %in% countil[[6]],2:ncol(tableData)]

#### NewTableData[[7]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[8 : 39]){
  counti[[k]]<-week
  k<-k+1
}
countil[[7]]<-counti  
NewtableData[[7]]<-tableData[tableData$week %in% countil[[7]],2:ncol(tableData)]

#### NewTableData[[8]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[9 : 40]){
  counti[[k]]<-week
  k<-k+1
}
countil[[8]]<-counti  
NewtableData[[8]]<-tableData[tableData$week %in% countil[[8]],2:ncol(tableData)]

#### NewTableData[[9]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[10 : 41]){
  counti[[k]]<-week
  k<-k+1
}
countil[[9]]<-counti  
NewtableData[[9]]<-tableData[tableData$week %in% countil[[9]],2:ncol(tableData)]

#### NewTableData[[10]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[11 : 42]){
  counti[[k]]<-week
  k<-k+1
}
countil[[10]]<-counti  
NewtableData[[10]]<-tableData[tableData$week %in% countil[[10]],2:ncol(tableData)]

#### NewTableData[[11]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[12 : 43]){
  counti[[k]]<-week
  k<-k+1
}
countil[[11]]<-counti  
NewtableData[[11]]<-tableData[tableData$week %in% countil[[11]],2:ncol(tableData)]

#### NewTableData[[12]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[13 : 44]){
  counti[[k]]<-week
  k<-k+1
}
countil[[12]]<-counti  
NewtableData[[12]]<-tableData[tableData$week %in% countil[[12]],2:ncol(tableData)]

#### NewTableData[[13]]
k=1
counti<-vector()
for(week in unique(tableDataweek)[14 : 45]){
  counti[[k]]<-week
  k<-k+1
}
countil[[13]]<-counti  
NewtableData[[13]]<-tableData[tableData$week %in% countil[[13]],2:ncol(tableData)]
#### timingwindow for 13 weeks####

for(i in 1:12){
  
  RFM_tablewa<-createRFMTable(NewtableData[[i]])  
  
  RFM_tablew[[i]]<- ordinalScore(RFM_tablewa,5)
  
  kmw[[i]]<-findBestKMeans(RFM_tablew[[i]])
  
  wi[[i]]<-findCenters(kmw[[i]])# centers of k means w0
  
# New cluster coding -----------------  
  kmclust2<-vector()
  kmclust2[1:length(kmw[[i]]$cluster)]<-0
  kmwnewcent<-vector()
  
  kmwnewcent[1:4]=0
  for(m in 1:4){
    if (kmw[[i]]$centers[m, "R"]>=3.5 && kmw[[i]]$centers[m, "f"]<2.5){
      kmwnewcent[m]=10   
    } else if (kmw[[i]]$centers[m, "R"]<3.5 && kmw[[i]]$centers[m, "f"]<=2.5){
      kmwnewcent[m]=20 
    } else if (kmw[[i]]$centers[m, "R"]>=3.5 && kmw[[i]]$centers[m, "f"]>2.5){
      kmwnewcent[m]=30   
    } else {
      kmwnewcent[m]=40  
    }
  }
  
  kmw[[i]]$centers<-cbind(kmw[[i]]$centers, newcent=kmwnewcent) 
  
  
  for (k in 1:4){
    if (wi[[i]][[k,1]]>=3.5 && wi[[i]][[k,2]]<2.5){
      kmclust2[kmw[[i]]$cluster==k]<-10
    } else if (wi[[i]][[k,1]]<3.5 && wi[[i]][[k,2]]<2.5){
      kmclust2[kmw[[i]]$cluster==k]<-20
    } else if (wi[[i]][[k,1]]>3.5 && wi[[i]][[k,2]]>2.5){
      kmclust2[kmw[[i]]$cluster==k]<-30
    } else {
      kmclust2[kmw[[i]]$cluster==k]<-40
    }
  }
  kmw[[i]]$clnew<-kmclust2
  # finish cluster coding  
  RFM_tablew[[i]]<-cbind(RFM_tablew[[i]], kmw[[i]]$cluster, kmw[[i]]$clnew)# add cluster of any CID to RFM_table
  
  clusterfreqw[[i]]<-c(length(kmw[[i]]$clnew[kmw[[i]]$clnew==10]),
                       length(kmw[[i]]$clnew[kmw[[i]]$clnew==20]),
                       length(kmw[[i]]$clnew[kmw[[i]]$clnew==30]),
                       length(kmw[[i]]$clnew[kmw[[i]]$clnew==40]))#frequency of every 5 clusters
  
  c1RFMw<-RFM_tablew[[i]][kmw[[i]]$clnew==10,] ;c2RFMw<-RFM_tablew[[i]][kmw[[i]]$clnew==20,] ;c3RFMw<-RFM_tablew[[i]][kmw[[i]]$clnew==30,] ;c4RFMw<-RFM_tablew[[i]][kmw[[i]]$clnew==40,]#divide RFM to clusters
  
  CRFMw[[i]]<-list(c1RFMw, c2RFMw, c3RFMw, c4RFMw)
  
  c1centerw<-kmw[[i]]$centers[kmw[[i]]$centers[,4]==10, ] ; c2centerw<-kmw[[i]]$centers[kmw[[i]]$centers[,4]==20, ] ; c3centerw<-kmw[[i]]$centers[kmw[[i]]$centers[,4]==30, ] ; c4centerw<-kmw[[i]]$centers[kmw[[i]]$centers[,4]==40, ]#fine centers of clusters
  c1centerww[[i]]<-list(c1centerw, c2centerw, c3centerw, c4centerw)
  
  CidClustwi<-cbind(RFM_tablew[[i]]$CId, kmw[[i]]$cluster, kmw[[i]]$clnew)#create data frame contain CID and cluster
  
  cidclust1wi<-CidClustwi[CidClustwi[,3] ==10,] ; cidrfmclust1wi<-RFM_tablew[[i]][kmw[[i]]$clnew ==10, ]#divide cid as clusters and rfm as clusters
  cidclust2wi<-CidClustwi[CidClustwi[,3] ==20,] ; cidrfmclust2wi<-RFM_tablew[[i]][kmw[[i]]$clnew ==20, ]
  cidclust3wi<-CidClustwi[CidClustwi[,3] ==30,] ; cidrfmclust3wi<-RFM_tablew[[i]][kmw[[i]]$clnew ==30, ]
  cidclust4wi<-CidClustwi[CidClustwi[,3] ==40,] ; cidrfmclust4wi<-RFM_tablew[[i]][kmw[[i]]$clnew ==40, ]
  
  cidclustwwi[[i]]<-list(cidclust1wi, cidclust2wi, cidclust3wi, cidclust4wi)
  cidrfmclustwwi[[i]]<-list(cidrfmclust1wi, cidrfmclust2wi, cidrfmclust3wi, cidrfmclust4wi)
  
  
  distwiallc1<-vector() ; distwiallc2<-vector() ; distwiallc3<-vector() ; distwiallc4<-vector() 
  for(f in 1:dim(RFM_tablew[[i]][1])){
    distwiallc1[f]<-sqrt((RFM_tablew[[i]][f, "R"] - c1centerww[[i]][[1]][[1]])^2 + 
                           (RFM_tablew[[i]][f, "f"] - c1centerww[[i]][[1]][[2]])^2 + 
                           (RFM_tablew[[i]][f, "M"] - c1centerww[[i]][[1]][[3]])^2
    )
    distwiallc2[f]<-sqrt((RFM_tablew[[i]][f, "R"] - c1centerww[[i]][[2]][[1]])^2 + 
                           (RFM_tablew[[i]][f, "f"] - c1centerww[[i]][[2]][[2]])^2 + 
                           (RFM_tablew[[i]][f, "M"] - c1centerww[[i]][[2]][[3]])^2
    )
    distwiallc3[f]<-sqrt((RFM_tablew[[i]][f, "R"] - c1centerww[[i]][[3]][[1]])^2 + 
                           (RFM_tablew[[i]][f, "f"] - c1centerww[[i]][[3]][[2]])^2 + 
                           (RFM_tablew[[i]][f, "M"] - c1centerww[[i]][[3]][[3]])^2
    )
    distwiallc4[f]<-sqrt((RFM_tablew[[i]][f, "R"] - c1centerww[[i]][[4]][[1]])^2 + 
                           (RFM_tablew[[i]][f, "f"] - c1centerww[[i]][[4]][[2]])^2 + 
                           (RFM_tablew[[i]][f, "M"] - c1centerww[[i]][[4]][[3]])^2
    )
  }
  
  
  
  distallclww[[i]]<-as.data.frame(cbind(RFM_tablew[[i]][[1]], distwiallc1, distwiallc2, distwiallc3, distwiallc4))
  
  
  distwic1<-vector() ; distwic2<-vector() ; distwic3<-vector() ; distwic4<-vector()
  
  
  for(j in 1:clusterfreqw[[i]][[1]]){
    distwic1[j]<-sqrt((cidrfmclust1wi[j, "R"] - c1centerww[[i]][[1]][[1]])^2 + 
                        (cidrfmclust1wi[j, "f"] - c1centerww[[i]][[1]][[2]])^2 + 
                        (cidrfmclust1wi[j, "M"] - c1centerww[[i]][[1]][[3]])^2)}
  
  for(j in 1:clusterfreqw[[i]][[2]]){
    distwic2[j]<-sqrt((cidrfmclust2wi[j, "R"] - c1centerww[[i]][[2]][[1]])^2 +
                        (cidrfmclust2wi[j, "f"] - c1centerww[[i]][[2]][[2]])^2 + 
                        (cidrfmclust2wi[j, "M"] - c1centerww[[i]][[2]][[3]])^2)}
  
  for(j in 1:clusterfreqw[[i]][[3]]){
    distwic3[j]<-sqrt((cidrfmclust3wi[j, "R"] - c1centerww[[i]][[3]][[1]])^2 + 
                        (cidrfmclust3wi[j, "f"] - c1centerww[[i]][[3]][[2]])^2 + 
                        (cidrfmclust3wi[j, "M"] - c1centerww[[i]][[3]][[3]])^2)}
  
  for(j in 1:clusterfreqw[[i]][[4]]){
    distwic4[j]<-sqrt((cidrfmclust4wi[j, "R"] - c1centerww[[i]][[4]][[1]])^2 + 
                        (cidrfmclust4wi[j, "f"] - c1centerww[[i]][[4]][[2]])^2 + 
                        (cidrfmclust4wi[j, "M"] - c1centerww[[i]][[4]][[3]])^2)}
  distwic[[i]]<-list(distwic1, distwic2, distwic3, distwic4)
  
  ciddistc1wi<-as.data.frame(cbind(CRFMw[[i]][[1]][[1]], distwic[[i]][[1]]))
  ciddistc1wi<-ciddistc1wi[order(ciddistc1wi[,2]),]
  
  ciddistc2wi<-as.data.frame(cbind(CRFMw[[i]][[2]][[1]], distwic[[i]][[2]]))
  ciddistc2wi<-ciddistc2wi[order(ciddistc2wi[,2]),]
  
  ciddistc3wi<-as.data.frame(cbind(CRFMw[[i]][[3]][[1]], distwic[[i]][[3]]))
  ciddistc3wi<-ciddistc3wi[order(ciddistc3wi[,2]),]
  
  ciddistc4wi<-as.data.frame(cbind(CRFMw[[i]][[4]][[1]], distwic[[i]][[4]]))
  ciddistc4wi<-ciddistc4wi[order(ciddistc4wi[,2]),]
  
  ciddistwi[[i]]<-data.frame(rbind(ciddistc1wi, ciddistc2wi, ciddistc3wi, ciddistc4wi))
  
  ciddistclwi[[i]]<-list(ciddistc1wi, ciddistc2wi, ciddistc3wi, ciddistc4wi)
}

#### shiny server ####
shinyServer(function(input, output, session){
  options(scipen=999)
#### overview panel1 analysis####
  
  clusterscolor = c("#337ab7", "#CC3399" , "#269e26" , "#FF6F00")  
  output$bartotal<-
    renderPlotly({
      dat2<-data.frame(
        clusters<-factor(c("New", "Passed", "VIP", "Churn"), levels = c("New", "Passed", "VIP", "Churn")),
        frequency<-c(clusterfreq[1], clusterfreq[2], clusterfreq[3], clusterfreq[4])
      )
      # none <- theme_blank()
      
      ggplot(data = dat2, aes(x=clusters, y=frequency, fill=clusters))+
        geom_bar(stat = "identity", fill= clusterscolor , width = 2) + geom_text(label = paste0(frequency),  colour="Black", family="xkcd-Regular", size = 5, show.legend = F)+
        theme(axis.line = element_line(size=1, colour = "black"), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.border = element_blank(), panel.background = element_blank()) +
        theme(plot.title=element_text(family="xkcd-Regular"), text=element_text(family="xkcd-Regular"), 
              axis.text.x=element_text(colour="black", size = 10), 
              axis.text.y=element_text(colour="black", size = 10))+
        theme_bw()
      # +opts(panel.background = theme_rect(fill='white', colour='black')) + opts(panel.grid.major = none, panel.grid.minor = none)
      # + ggtitle("Frequency of Clusters")
      # +labs(x="", y="")
      ggplotly()
    })

  c1center<-round(c1center, digits = 2)
  output$bartoo<-
    renderPlotly({
      dat3<-data.frame(
        fila=rep(c("Mean", "Total"), c(3,3)),
        x=rep(c("R", "F", "M"), 2),
        y=c(c1center[[1]], c1center[[2]], c1center[[3]], 5-c1center[[1]],
            5-c1center[[2]], 5-c1center[[3]]),
        label=c(c1center[[1]], c1center[[2]], c1center[[3]], 5, 5, 5)
      )
      dat3 <- ddply(dat3, .(x),
                    transform, pos = cumsum(y) - (0.5 * y))

      filli <- c(clusterscolor[[1]], "#3F3F3F")
      position<-c("R", "F", "M")
      p <- ggplot()+theme_bw() + scale_x_discrete(limits = position) +geom_bar(aes(y = y, x = x, fill = fila), data = dat3,
                               stat="identity") +
        labs(x="", y="")  +scale_fill_manual(values=filli) +
        geom_text(data=dat3, aes(x = x, y = pos,
                                 label = paste0(label)), size=3) +
        theme(legend.position="none", legend.title = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank())+coord_flip()
      p

    })

  c2center<-round(c2center, digits = 2)
  output$bartot<-
    renderPlotly({
      dat3<-data.frame(
        fila=rep(c("Mean", "Total"), c(3,3)),
        x=rep(c("R", "F", "M"), 2),
        y=c(c2center[[1]], c2center[[2]], c2center[[3]], 5-c2center[[1]],
            5-c2center[[2]], 5-c2center[[3]]),
        label=c(c2center[[1]], c2center[[2]], c2center[[3]], 5, 5, 5)
      )
      dat3 <- ddply(dat3, .(x),
                    transform, pos = cumsum(y) - (0.5 * y))

      filli <- c(clusterscolor[[2]], "#3F3F3F")
      position<-c("R", "F", "M")
      p <- ggplot()+ theme_bw() +scale_x_discrete(limits = position) +geom_bar(aes(y = y, x = x, fill = fila), data = dat3,
                                     stat="identity") +
        labs(x="", y="") +  scale_fill_manual(values=filli) +
        geom_text(data=dat3, aes(x = x, y = pos,
                                 label = paste0(label)), size=3) +
        theme(legend.position="none")+coord_flip()
      p

    })

  c3center<-round(c3center, digits = 2)
  output$bartoth<-
    renderPlotly({
      dat3<-data.frame(
        fila=rep(c("Mean", "Total"), c(3,3)),
        x=rep(c("R", "F", "M"), 2),
        y=c(c3center[[1]], c3center[[2]], c3center[[3]], 5-c3center[[1]],
            5-c3center[[2]], 5-c3center[[3]]),
        label=c(c3center[[1]], c3center[[2]], c3center[[3]], 5, 5, 5)
      )
      dat3 <- ddply(dat3, .(x),
                    transform, pos = cumsum(y) - (0.5 * y))

      filli <- c(clusterscolor[[3]], "#3F3F3F")
      position<-c("R", "F", "M")
      p <- ggplot()+theme_bw() + scale_x_discrete(limits = position) +geom_bar(aes(y = y, x = x, fill = fila), data = dat3,
                                             stat="identity") +
        labs(x="", y="")  + scale_fill_manual(values=filli) +
        geom_text(data=dat3, aes(x = x, y = pos,
                                 label = paste0(label)), size=3) +
        theme(legend.position="none")+coord_flip()
      p

    })

  c4center<-round(c4center, digits = 2)
  output$bartofr<-
    renderPlotly({
      dat3<-data.frame(
        fila=rep(c("Mean", "Total"), c(3,3)),
        x=rep(c("R", "F", "M"), 2),
        y=c(c4center[[1]], c4center[[2]], c4center[[3]], 5-c4center[[1]],
            5-c4center[[2]], 5-c4center[[3]]),
        label=c(c4center[[1]], c4center[[2]], c4center[[3]], 5, 5, 5)
      )
      dat3 <- ddply(dat3, .(x),
                    transform, pos = cumsum(y) - (0.5 * y))

      filli <- c(clusterscolor[[4]], "#3F3F3F")
      position<-c("R", "F", "M")
      p <- ggplot()+theme_bw() + scale_x_discrete(limits = position) +geom_bar(aes(y = y, x = x, fill = fila), data = dat3,
                                                 stat="identity") +
        labs(x="", y="")  +  scale_fill_manual(values=filli) +
        geom_text(data=dat3, aes(x = x, y = pos,
                                 label = paste0(label)), size=3) +
        theme(legend.position="none")+coord_flip()
      p

    })
#### Cluster New RFM distribution analysis ####
  output$text1 <- renderText({ paste("The NEW cluster Customers : who have bought recently but in small quantities")})
  output$text2 <-renderText({ paste("The PASSED cluster Customers : who have not bought recently but in small quantities")})
  output$text3 <-renderText({paste("The VIP Cluster Customers : who have bought recently and in large quantities")})
  output$text4 <-renderText({paste("The CHURN Cluster Customers : who have not bought recently but in large quantities")})
  
  
  output$barRclone<-
    renderPlotly({
      dat3<-data.frame(
        R=factor(c(names(table(CRFMw[[12]][[1]][[2]])[1:length(table(CRFMw[[12]][[1]][[2]]))])),
                  levels = c(names(table(CRFMw[[12]][[1]][[2]]))[1:length(table(CRFMw[[12]][[1]][[2]]))])),
        frequency = c(table(CRFMw[[12]][[1]][[2]])[1:length(table(CRFMw[[12]][[1]][[2]]))]))
      
      ggplot(data = dat3, aes(x=R, y=frequency, fill=R))+theme_bw()+
        geom_bar(stat = "identity",  fill=clusterscolor[[1]], colour=clusterscolor[[1]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })


  output$barFclone<-
    renderPlotly({
      dat3<-data.frame(
        F=factor(c(names(table(CRFMw[[12]][[1]][[3]])[1:length(table(CRFMw[[12]][[1]][[3]]))])),
                  levels = c(names(table(CRFMw[[12]][[1]][[3]]))[1:length(table(CRFMw[[12]][[1]][[3]]))])),
        frequency = c(table(CRFMw[[12]][[1]][[3]])[1:length(table(CRFMw[[12]][[1]][[3]]))]))
dat3<-data.frame(F=c(1,2,3,4,5), frequency=c(183, 0 ,5, 0, 0))

      ggplot(data = dat3, aes(x=F, y=frequency, fill=F))+ theme_bw()+
        geom_bar(stat = "identity",  fill=clusterscolor[[1]], colour=clusterscolor[[1]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })



  output$barMclone<-
    renderPlotly({
      dat3<-data.frame(
        M=factor(c(names(table(CRFMw[[12]][[1]][[4]])[1:length(table(CRFMw[[12]][[1]][[4]]))])),
                  levels = c(names(table(CRFMw[[12]][[1]][[4]]))[1:length(table(CRFMw[[12]][[1]][[4]]))])),
        frequency = c(table(CRFMw[[12]][[1]][[4]])[1:length(table(CRFMw[[12]][[1]][[4]]))]))

      ggplot(data = dat3, aes(x=M, y=frequency, fill=M))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[1]], colour=clusterscolor[[1]]) +scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })
#### Cluster Passed RFM distribution analysis ####
  
  output$barRcltwo<-
    renderPlotly({
      dat3<-data.frame(
        R=factor(c(names(table(CRFMw[[12]][[2]][[2]])[1:length(table(CRFMw[[12]][[2]][[2]]))])),
                  levels = c(names(table(CRFMw[[12]][[2]][[2]]))[1:length(table(CRFMw[[12]][[2]][[2]]))])),
        frequency = c(table(CRFMw[[12]][[2]][[2]])[1:length(table(CRFMw[[12]][[2]][[2]]))]))
dat3<-data.frame(R=c(1,2,3,4,5), frequency=c(149, 120, 0 , 0, 0))
      ggplot(data = dat3, aes(x=R, y=frequency, fill=R))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[2]], colour=clusterscolor[[2]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })


  output$barFcltwo<-
    renderPlotly({

      dat3<-data.frame(
        F=factor(c(names(table(CRFMw[[12]][[2]][[3]])[1:length(table(CRFMw[[12]][[2]][[3]]))])),
                  levels = c(names(table(CRFMw[[12]][[2]][[3]]))[1:length(table(CRFMw[[12]][[2]][[3]]))])),
        frequency = c(table(CRFMw[[12]][[2]][[3]])[1:length(table(CRFMw[[12]][[2]][[3]]))]))
dat3<-data.frame(F=c(1,2,3,4,5), frequency=c(257, 0, 12, 0, 0))
      ggplot(data = dat3, aes(x=F, y=frequency, fill=F))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[2]], colour=clusterscolor[[2]])+scale_x_discrete(limits=c(1,2,3,4,5)) + scale_y_continuous(limits = c(0,300))
      
      ggplotly()
    })



  output$barMcltwo<-
    renderPlotly({
      dat3<-data.frame(
        M<-factor(c(names(table(CRFMw[[12]][[2]][[4]])[1:length(table(CRFMw[[12]][[2]][[4]]))])),
                  levels = c(names(table(CRFMw[[12]][[2]][[4]]))[1:length(table(CRFMw[[12]][[2]][[4]]))])),
        frequency = c(table(CRFMw[[12]][[2]][[4]])[1:length(table(CRFMw[[12]][[2]][[4]]))]))

      ggplot(data = dat3, aes(x=M, y=frequency, fill=M))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[2]], colour=clusterscolor[[2]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })
#### Cluster VIP RFM distribution analysis ####
  
  output$barRclthree<-
    renderPlotly({
      dat3<-data.frame(
        R<-factor(c(names(table(CRFMw[[12]][[3]][[2]])[1:length(table(CRFMw[[12]][[3]][[2]]))])),
                  levels = c(names(table(CRFMw[[12]][[3]][[2]]))[1:length(table(CRFMw[[12]][[3]][[2]]))])),
        frequency = c(table(CRFMw[[12]][[3]][[2]])[1:length(table(CRFMw[[12]][[3]][[2]]))]))

      ggplot(data = dat3, aes(x=R, y=frequency, fill=R))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[3]], colour=clusterscolor[[3]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })


  output$barFclthree<-
    renderPlotly({

      dat3<-data.frame(
        F<-factor(c(names(table(CRFMw[[12]][[3]][[3]])[1:length(table(CRFMw[[12]][[3]][[3]]))])),
                  levels = c(names(table(CRFMw[[12]][[3]][[3]]))[1:length(table(CRFMw[[12]][[3]][[3]]))])),
        frequency = c(table(CRFMw[[12]][[3]][[3]])[1:length(table(CRFMw[[12]][[3]][[3]]))]))


      ggplot(data = dat3, aes(x=F, y=frequency, fill=F))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[3]], colour=clusterscolor[[3]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })



  output$barMclthree<-
    renderPlotly({
      dat3<-data.frame(
        M<-factor(c(names(table(CRFMw[[12]][[3]][[4]])[1:length(table(CRFMw[[12]][[3]][[4]]))])),
                  levels = c(names(table(CRFMw[[12]][[3]][[4]]))[1:length(table(CRFMw[[12]][[3]][[4]]))])),
        frequency = c(table(CRFMw[[12]][[3]][[4]])[1:length(table(CRFMw[[12]][[3]][[4]]))]))

      ggplot(data = dat3, aes(x=M, y=frequency, fill=M))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[3]], colour=clusterscolor[[3]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })
#### Cluster CHURN RFM distribution analysis ####
  
  output$barRclfour<-
    renderPlotly({
      dat3<-data.frame(
        R<-factor(c(names(table(CRFMw[[12]][[4]][[2]])[1:length(table(CRFMw[[12]][[4]][[2]]))])),
                  levels = c(names(table(CRFMw[[12]][[4]][[2]]))[1:length(table(CRFMw[[12]][[4]][[2]]))])),
        frequency = c(table(CRFMw[[12]][[4]][[2]])[1:length(table(CRFMw[[12]][[4]][[2]]))]))

      ggplot(data = dat3, aes(x=R, y=frequency, fill=R))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[4]], colour=clusterscolor[[4]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })


  output$barFclfour<-
    renderPlotly({

      dat3<-data.frame(
        F<-factor(c(names(table(CRFMw[[12]][[4]][[3]])[1:length(table(CRFMw[[12]][[4]][[3]]))])),
                  levels = c(names(table(CRFMw[[12]][[4]][[3]]))[1:length(table(CRFMw[[12]][[4]][[3]]))])),
        frequency = c(table(CRFMw[[12]][[4]][[3]])[1:length(table(CRFMw[[12]][[4]][[3]]))]))


      ggplot(data = dat3, aes(x=F, y=frequency, fill=F))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[4]], colour=clusterscolor[[4]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })



  output$barMclfour<-
    renderPlotly({
      dat3<-data.frame(
        M<-factor(c(names(table(CRFMw[[12]][[4]][[4]])[1:length(table(CRFMw[[12]][[4]][[4]]))])),
                  levels = c(names(table(CRFMw[[12]][[4]][[4]]))[1:length(table(CRFMw[[12]][[4]][[4]]))])),
        frequency = c(table(CRFMw[[12]][[4]][[4]])[1:length(table(CRFMw[[12]][[4]][[4]]))]))

      ggplot(data = dat3, aes(x=M, y=frequency, fill=M))+
        theme_bw()+geom_bar(stat = "identity",  fill=clusterscolor[[4]], colour=clusterscolor[[4]])+scale_x_discrete(limits=c(1,2,3,4,5))+ scale_y_continuous(limits = c(0,300))
      ggplotly()
    })
#### create list of dist, v ,a for every cluster - server ####
  dvasortw12<-list()

  for(i in 1:4){
    dist<-vector()
    v<-vector()
    a<-vector()

    for(k in 1: length(ciddistclwi[[12]][[i]][[1]])){

      if(ciddistclwi[[12]][[i]][[k, 1]] %in% ciddistwi[[11]][[1]]){

        v[k] <- ciddistclwi[[12]][[i]][[k, 2]]- ciddistwi[[11]][ciddistwi[[11]][[1]]==ciddistclwi[[12]][[i]][[1]][k], 2]

        if(ciddistclwi[[12]][[i]][[k, 1]] %in% ciddistwi[[10]][[1]]){

          a[k] <- ciddistclwi[[12]][[i]][[k, 2]]-2*ciddistwi[[11]][ciddistwi[[11]][[1]]==ciddistclwi[[12]][[i]][[1]][k], 2]+ciddistwi[[10]][ciddistwi[[10]][[1]]==ciddistclwi[[12]][[i]][[1]][k], 2]

        } else {
          a[k]   <- ciddistclwi[[12]][[i]][[k, 2]] - ciddistwi[[11]][ciddistwi[[11]][[1]]==ciddistclwi[[12]][[i]][[1]][k], 2]
        }
      } else {
        v[k]=0
        a[k]=0
      }
    }
    dvasortw12[[i]]<-as.data.frame(cbind(CID=ciddistclwi[[12]][[i]][[1]], Dist=ciddistclwi[[12]][[i]][[2]], V=v, A=a))

  }
  
#### 3 first cid in dist,v ,a for VIP cluster - server ####  
  
  
  dcl3w12<-dvasortw12[[3]][order(dvasortw12[[3]][[2]]), ]
  vcl3w12<-dvasortw12[[3]][order(dvasortw12[[3]][[3]], decreasing = FALSE), ]
  acl3w12<-dvasortw12[[3]][order(dvasortw12[[3]][[4]], decreasing = FALSE), ]

  output$distcthreeone<-renderPrint({dcl3w12[1,1]})
  output$distcthreetwo<-renderPrint({dcl3w12[2,1]})
  output$distcthreethree<-renderPrint({dcl3w12[3,1]})


  output$vcthreeone<-renderPrint({vcl3w12[1,1]})
  output$vcthreetwo<-renderPrint({vcl3w12[2,1]})
  output$vcthreethree<-renderPrint({vcl3w12[3,1]})


  output$acthreeone<-renderPrint({acl3w12[1,1]})
  output$acthreetwo<-renderPrint({acl3w12[2,1]})
  output$acthreethree<-renderPrint({acl3w12[3,1]})
  
#### 3 first cid in dist,v ,a for Churn cluster - server ####
  
  
  dcl4w12<-dvasortw12[[4]][order(dvasortw12[[4]][[2]]), ]
  vcl4w12<-dvasortw12[[4]][order(dvasortw12[[4]][[3]], decreasing = FALSE), ]
  acl4w12<-dvasortw12[[4]][order(dvasortw12[[4]][[4]], decreasing = FALSE), ]

  output$distcfourone<-
    renderPrint({dcl4w12[1,1]})
  output$distcfourtwo<-
    renderPrint({dcl4w12[2,1]})
  output$distcfourthree<-
    renderPrint({dcl4w12[3,1]})


  output$vcfourone<-
    renderPrint({vcl4w12[1,1]})
  output$vcfourtwo<-
    renderPrint({vcl4w12[2,1]})
  output$vcfourthree<-
    renderPrint({vcl4w12[3,1]})


  output$acfourone<-
    renderPrint({acl4w12[1,1]})
  output$acfourtwo<-
    renderPrint({acl4w12[2,1]})
  output$acfourthree<-
    renderPrint({acl4w12[3,1]})
  


#   
#     
# #### update CID import for clusters ####
observe({
  if(input$chclust == "New"){
      updateSelectInput(session,
                        "num1",
                        choices = cidrfmclustwwi[[12]][[1]][[1]])

}

  if(input$chclust == "Passed") {
    updateSelectInput(session,
                      "num1",
                      choices = cidrfmclustwwi[[12]][[2]][[1]])
}
  if(input$chclust == "VIP"){
      updateSelectInput(session,
                        "num1",
                        choices = cidrfmclustwwi[[12]][[3]][[1]])
    }

  if(input$chclust == "Churn"){
        updateSelectInput(session,
                          "num1",
                          choices = cidrfmclustwwi[[12]][[4]][[1]])
      }
})

  num11<-eventReactive(input$action1,{
    input$num1
  })


#### make reactive inputs from CID's ####
#  
# 
# #### Outputs in NEW Cluster ####
#   
value11<-reactive({
    value<-vector()

    for (i in 1:12){
      value[i]<-as.numeric(distallclww[[i]][distallclww[[i]][[1]] == as.numeric(num11()), 4])
    }

    row<-c(1:length(value))
    value<-cbind(row, value)
    value<-as.data.frame(value)
    return(value)
  })


observeEvent(input$action1, {

output$vecplotc11<-renderDygraph({


    dygraph(value11()[1 : dim(value11())[1], ], main = "Distance from VIP Cluster center", xlab = "week")%>%
    dyAxis("y", label = "Distance", valueRange = c(0, 7)) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, drawGapEdgePoints = 1, axisLineColor = "navy", gridLineWidth = 2, pointSize=2) %>%
    dyLegend(show = "follow")
  })
})

value12<-reactive({
  value<-vector()

    for (i in 1:12){
      value[i]<-as.numeric(distallclww[[i]][distallclww[[i]][[1]] == as.numeric(num11()), 5])
    }

    row<-c(1:length(value))
    value<-cbind(row, value)
    value<-as.data.frame(value)
    return(value)
  })

observeEvent(input$action1, {
  output$vecplotc12<-renderDygraph({


    dygraph(value12()[1 : dim(value12())[1], ], main = "Distance from Churn Cluster center", xlab = "week") %>%
      dyAxis("y", label = "Distance", valueRange = c(0, 7)) %>%
      dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, drawGapEdgePoints = 4, axisLineColor = "navy", gridLineWidth = 2, pointSize=2) %>%
      dyLegend(show = "follow")
  })
})

  RFM1<-reactive({

    RFMVec<-vector()

    RFMVec["R"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== as.numeric(num11()), 2]
    RFMVec["F"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== as.numeric(num11()), 3]
    RFMVec["M"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== as.numeric(num11()), 4]

    RFM<-c(RFMVec["R"], RFMVec["F"], RFMVec["M"])
    RFM[1]=round(RFM[1], digits = 2)
    RFM[2]=round(RFM[2], digits = 2)
    RFM[3]=round(RFM[3], digits = 2)
    RFM
  })


output$r<-renderGvis({
    
    
    df<-data.frame(label = "R", value=RFM1()[1])
    gvisGauge(df,
              options=list(min=0, max=5, greenFrom=2.5,
                         greenTo=5,
                           redFrom=0, redTo=2.5, width=150, height=350));
    
  })
output$f<-renderGvis({
  
  
  df<-data.frame(label = "F", value=RFM1()[2])
  gvisGauge(df,
            options=list(min=0, max=5, greenFrom=3.5,
                         yellowFrom=2.5, yellowTo=3.5,
                         greenTo=5,
                         redFrom=0, redTo=2.5, width=150, height=350));
})
output$m<-renderGvis({
  
  
  df<-data.frame(label = "M", value=RFM1()[3])
  gvisGauge(df,
            options=list(min=0, max=5, greenFrom=2.5,
                         greenTo=5,
                         redFrom=0, redTo=2.5, width=150, height=350));
  
})


# output$RFMVecplot1<- renderPlotly({
# 
#     datar3<-data.frame(
#       fila=rep(c("Mean", "Total"), c(3,3)),
#       x=rep(c("R", "F", "M"), 2),
#       y=c(RFM1()[1], RFM1()[2], RFM1()[3], 5-RFM1()[1],
#           5-RFM1()[2], 5-RFM1()[3]),
#       label=c(RFM1()[1], RFM1()[2], RFM1()[3], 5, 5, 5)
#     )
#     dat3 <- ddply(datar3, .(x),
#                   transform, pos = cumsum(y) - (0.5 * y))
# 
#     filli <- c("#337ab7", "#666C3F")
#     position<-c("R", "F", "M")
#     p <- ggplot()+theme_bw() + scale_x_discrete(limits = position) +geom_bar(aes(y = y, x = x, fill = fila), data = dat3,
#                                                                              stat="identity") +
#       labs(x="", y="")  +scale_fill_manual(values=filli) +
#       geom_text(data=dat3, aes(x = x, y = pos,
#                                label = paste0(label)), size=3) +
#       ggtitle(paste("RFM For", num11(), "CID")) +
#       theme(legend.position="none", legend.title = element_blank(),
#             panel.grid.minor = element_blank(),
#             panel.grid.major = element_blank(),
#             panel.background = element_blank(),
#             plot.background = element_blank())
#     p
# 
#   })


  output$radar1<-renderChartJSRadar({

    labs <- c("NEW", "PASSED", "VIP", "CHURN")

    scores<-list(
      "radar1"=c(distallclww[[12]][distallclww[[12]][1] == as.numeric(num11()), 2],
                 distallclww[[12]][distallclww[[12]][1] == as.numeric(num11()), 3],
                 distallclww[[12]][distallclww[[12]][1] == as.numeric(num11()), 4],
                 distallclww[[12]][distallclww[[12]][1] == as.numeric(num11()), 5]))


    chartJSRadar(scores = scores, labs = labs, maxScale = 10)

  })
  

  
#### Outputs in PASSED Cluster ####
  
#   vec2<-reactive({
#     vec<-vector()
#     
#     for (i in 1:12){
#       vec[i]<-as.numeric(distallclww[[i]][distallclww[[i]][[1]] == num22(), 2])
#       # ciddistwi[[i]][ciddistwi[[i]][[1]] == input$num, 2]
#     }
#     
#     row<-c(1:length(vec))
#     vec<-cbind(row, vec)
#     vec<-as.data.frame(vec)
#     return(vec)
#   })  
#   
#   
#   output$vecplotc2<-renderDygraph({
#     
#     
#     dygraph(vec2()[1 : dim(vec2())[1], ], main = "time seri plot", xlab = "week")%>% dyAxis("y", label = "Distance", valueRange = c(1, 10))
#   })
#   
#   
#   RFM2<-reactive({
#     
#     RFMVec<-vector()
#     
#     RFMVec["R"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num22(), 2]    
#     RFMVec["F"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num22(), 3]
#     RFMVec["M"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num22(), 4]
#     
#     RFM<-c(RFMVec["R"], RFMVec["F"], RFMVec["M"])
#     RFM
#     
#   })
#   
#   
#   
#   
#   
#   output$RFMVecplot2<- renderPlotly({
#     
#     datar3<-data.frame(
#       centers<-factor(c("R", "F", "M"), levels = c("R", "F", "M")),
#       frequency = c(RFM2()[1],RFM2()[2],RFM2()[3]))
#     
#     
#     ggplot(data = datar3, aes(x=centers, y=frequency, fill=centers))+ geom_bar(stat = "identity",  fill="blue", colour="black")
#     
#     ggplotly()
#     
#   })
#   
#   
#   dataradar2<-reactive({
#     datar<-data.frame()
#     datar<-cbind(t(RFM_tablew[[12]][RFM_tablew[[12]][1]==num22(), 2:4]),
#                  c1=c1centerww[[12]][[1]][-4],c2=c1centerww[[12]][[2]][-4],
#                  c3=c1centerww[[12]][[3]][-4], c4=c1centerww[[12]][[4]][-4])
#     colnames(datar)=c("y", "C1", "C2", "C3", "C4")
#     datar<-cor(datar)
#     return(datar)
#     
#   })
#   
#   output$radar2<-renderPlot({
#     frame<-dataradar2()
#     
#     spider(y=1, x=2:5, data=frame, overlay = TRUE,
#            scale = 0.25, ncolors = 3, main = "Distance Spider Plot"
#     )
#   })
#   
#   
#   
#   
# #### Outputs in VIP Cluster ####
#   
#   vec3<-reactive({
#     vec<-vector()
#     
#     for (i in 1:12){
#       vec[i]<-as.numeric(distallclww[[i]][distallclww[[i]][[1]] == num33(), 2])
#       # ciddistwi[[i]][ciddistwi[[i]][[1]] == input$num, 2]
#     }
#     
#     row<-c(1:length(vec))
#     vec<-cbind(row, vec)
#     vec<-as.data.frame(vec)
#     return(vec)
#   })  
#   
#   
#   output$vecplotc3<-renderDygraph({
#     
#     
#     dygraph(vec3()[1 : dim(vec3())[1], ], main = "time seri plot", xlab = "week", ylab = "Distance")
#   })
#   
#   
#   RFM3<-reactive({
#     
#     RFMVec<-vector()
#     
#     RFMVec["R"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 2]    
#     RFMVec["F"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 3]
#     RFMVec["M"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 4]
#     
#     RFM<-c(RFMVec["R"], RFMVec["F"], RFMVec["M"])
#     RFM
#     
#   })
#   
#   
#   
#   
#   
#   output$RFMVecplot3<- renderPlotly({
#     
#     datar3<-data.frame(
#       centers<-factor(c("R", "F", "M"), levels = c("R", "F", "M")),
#       frequency = c(RFM3()[1],RFM3()[2],RFM3()[3]))
#     
#     
#     ggplot(data = datar3, aes(x=centers, y=frequency, fill=centers))+ geom_bar(stat = "identity",  fill="blue", colour="black")
#     
#     ggplotly()
#     
#   })
#   
#   
#   dataradar3<-reactive({
#     datar<-data.frame()
#     datar<-cbind(t(RFM_tablew[[12]][RFM_tablew[[12]][1]==num33(), 2:4]),
#                  c1=c1centerww[[12]][[1]][-4],c2=c1centerww[[12]][[2]][-4],
#                  c3=c1centerww[[12]][[3]][-4], c4=c1centerww[[12]][[4]][-4])
#     colnames(datar)=c("y", "C1", "C2", "C3", "C4")
#     datar<-cor(datar)
#     return(datar)
#     
#   })
#   
#   output$radar3<-renderPlot({
#     frame<-dataradar3()
#     
#     spider(y=1, x=2:5, data=frame, overlay = TRUE,
#            scale = 0.25, ncolors = 3, main = "Distance Spider Plot"
#     )
#   })
#   
# #### Outputs in CHURN Cluster ####
#   
#   vec4<-reactive({
#     vec<-vector()
#     
#     for (i in 1:12){
#       vec[i]<-as.numeric(distallclww[[i]][distallclww[[i]][[1]] == num44(), 2])
#       # ciddistwi[[i]][ciddistwi[[i]][[1]] == input$num, 2]
#     }
#     
#     row<-c(1:length(vec))
#     vec<-cbind(row, vec)
#     vec<-as.data.frame(vec)
#     return(vec)
#   })  
#   
#   
#   output$vecplotc4<-renderDygraph({
#     
#     
#     dygraph(vec4()[1 : dim(vec4())[1], ], main = "time seri plot", xlab = "week", ylab = "Distance")
#   })
#   
#   
#   RFM4<-reactive({
#     
#     RFMVec<-vector()
#     
#     RFMVec["R"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 2]    
#     RFMVec["F"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 3]
#     RFMVec["M"]<-RFM_tablew[[12]][RFM_tablew[[12]][1]== num11(), 4]
#     
#     RFM<-c(RFMVec["R"], RFMVec["F"], RFMVec["M"])
#     RFM
#     
#   })
#   
#   
#   
#   
#   
#   output$RFMVecplot4<- renderPlotly({
#     
#     datar4<-data.frame(
#       centers<-factor(c("R", "F", "M"), levels = c("R", "F", "M")),
#       frequency = c(RFM1()[1],RFM1()[2],RFM1()[3]))
#     
#     
#     ggplot(data = datar3, aes(x=centers, y=frequency, fill=centers))+ geom_bar(stat = "identity",  fill="blue", colour="black")
#     
#     ggplotly()
#     
#   })
#   
#   
#   dataradar4<-reactive({
#     datar<-data.frame()
#     datar<-cbind(t(RFM_tablew[[12]][RFM_tablew[[12]][1]==num44(), 2:4]),
#                  c1=c1centerww[[12]][[1]][-4],c2=c1centerww[[12]][[2]][-4],
#                  c3=c1centerww[[12]][[3]][-4], c4=c1centerww[[12]][[4]][-4])
#     colnames(datar)=c("y", "C1", "C2", "C3", "C4")
#     datar<-cor(datar)
#     return(datar)
#     
#   })
#   
#   output$radar4<-renderPlot({
#     frame<-dataradar4()
#     
#     spider(y=1, x=2:5, data=frame, overlay = TRUE,
#            scale = 0.25, ncolors = 3, main = "Distance Spider Plot"
#     )
#   })
#   
#   
#   
  
  
  
#### Finish shiny server ####  
})