d<-read.table("C:/Users/deepak14035/Desktop/IRIS.csv",sep=",")
d$V9<-NULL
x<-prcomp(d)
s <- as.data.frame(x$x)
data<-s[,c(1,2)]
data
rando<-sample(1:100,4)#generate 4 random indices
rando
cx<-c(data[rando[1],1], data[rando[2],1], data[rando[3],1], data[rando[4],1])
cy<-c(data[rando[1],2], data[rando[2],2], data[rando[3],2], data[rando[4],2])

cxhistory<-runif(4,min(data[,1]), max(data[,1]))
cyhistory<-runif(4,min(data[,1]), max(data[,1]))
cl<-runif(12,min(data[,2]), max(data[,2]))
cl1<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
cl2<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
cl3<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
cl4<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
filled<-c(0,0,0,0)
rows<-nrow(data)
while(TRUE){
  cl1<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
  cl2<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
  cl3<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
  cl4<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
  filled<-c(0,0,0,0)  
  for(i in 1:rows){
    min<- 9999
    minx<- -1
    miny<- -1
    clust<- 1
    for(j in 1:4){
      dist<-sqrt(((data[i,1]-cx[j])*(data[i,1]-cx[j]))+((data[i,2]-cy[j])*(data[i,2]-cy[j])))
      if(min>dist){
        min<-dist
        clust<- j
      }
      
      
    }
    if(clust==1){
      filled[1]<-filled[1]+1
      cl1[filled[1],1]<-data[i,1]
      cl1[filled[1],2]<-data[i,2]
      
    }
    else if(clust==2){
      filled[2]<-filled[2]+1
      cl2[filled[2],1]<-data[i,1]
      cl2[filled[2],2]<-data[i,2]
    }
    else if(clust==3){
      filled[3]<-filled[3]+1
      cl3[filled[3],1]<-data[i,1]
      cl3[filled[3],2]<-data[i,2]
    }
    else if(clust==4){
      filled[4]<-filled[4]+1
      cl4[filled[4],1]<-data[i,1]
      cl4[filled[4],2]<-data[i,2]
    }
    
    #print(min)
  }
  cxhistory[1]<-cx[1]
  cyhistory[1]<-cy[1]
  cxhistory[2]<-cx[2]
  cyhistory[2]<-cy[2]
  cxhistory[3]<-cx[3]
  cyhistory[3]<-cy[3]
  cxhistory[4]<-cx[4]
  cyhistory[4]<-cy[4]
  
  cx[1]<-mean(cl1[,1], na.rm = TRUE)
  cy[1]<-mean(cl1[,2], na.rm = TRUE)
  cx[2]<-mean(cl2[,1], na.rm = TRUE)
  cy[2]<-mean(cl2[,2], na.rm = TRUE)
  cx[3]<-mean(cl3[,1], na.rm = TRUE)
  cy[3]<-mean(cl3[,2], na.rm = TRUE)
  cx[4]<-mean(cl4[,1], na.rm = TRUE)
  cy[4]<-mean(cl4[,2], na.rm = TRUE)
  totaldist<-0
  for(j in 1:4){
    totaldist<-totaldist+sqrt(((cxhistory[j]-cx[j])*(cxhistory[j]-cx[j]))+((cyhistory[j]-cy[j])*(cyhistory[j]-cy[j])))
    
  }
  if(totaldist<0.001){
    break
  }
  plot(c(-3, 4), c(-1.5, 1.5), col="white")
  lines(cl1[,1], cl1[,2], col="black")
  lines(cl2[,1], cl2[,2], col="red")
  lines(cl3[,1], cl3[,2], col="green")
  lines(cl4[,1], cl4[,2], col="blue")
}
