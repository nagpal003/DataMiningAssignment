d<-read.table("C:/Users/deepak14035/Desktop/IRIS.csv",sep=",")
d$V9<-NULL
x<-prcomp(d)
s <- as.data.frame(x$x)
data<-s[,c(1,2)]

cl<-list()

col=c("red","yellow","skyblue4","purple","pink","black","brown","green","grey","lightskyblue","darkgoldenrod","chocolate")
dm<-matrix( nrow = 12, ncol = 1, byrow = TRUE)
for(l in 1:11){
  dm[l]<-data[1,1]
}
dim(dm)
rows<-nrow(data)
inde<-1
for(k in 2:12){
  cxhistory<-runif(k,min(data[,1]), max(data[,1]))
  cyhistory<-runif(k,min(data[,1]), max(data[,1]))
  rando<-sample(1:100,k)#generate 4 random indices
  rando
  filled<-matrix( nrow = k, ncol = 1, byrow = TRUE)
  cx<-matrix( nrow = k, ncol = 1, byrow = TRUE)
  cy<-matrix( nrow = k, ncol = 1, byrow = TRUE)
  for(l in 1:k){
    cx[l]<-data[rando[l],1]
    cy[l]<-data[rando[l],2]
    filled[l]<-1
  }
  while(TRUE){
    for(l in 1:k){
      cl[[l]]<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
      filled[l]<-1
    }
    for(i in 1:rows){
      min<- 9999
      minx<- -1
      miny<- -1
      clust<- 1
      for(j in 1:k){
        dist<-sqrt(((data[i,1]-cx[j])*(data[i,1]-cx[j]))+((data[i,2]-cy[j])*(data[i,2]-cy[j])))
        
        if(min>dist){
          min<-dist
          clust<- j
        }
      }
      #print(filled)
      
      cl[[clust]][filled[clust],1]<-data[i,1]
      
      cl[[clust]][filled[clust],2]<-data[i,2]
      filled[clust]<-filled[clust]+1
      
      
      #print(min)
    }
    
    for(j in 1:k){
      cxhistory[j]<-cx[j]
      cyhistory[j]<-cy[j]
      if(filled[j]==1){
        ran<-sample(1:100, 1)
        cx[j]=data[ran[1],1]
        cy[j]=data[ran[1],2]
        
      }
      else{
        cx[j]<-mean(cl[[j]], na.rm=TRUE)
        cy[j]<-mean(cl[[j]], na.rm=TRUE)
      }
    }
    
    totaldist<- 0
    totaldist2<- 0
    for(j in 1:k){
      
      totaldist<-totaldist+sqrt(((cxhistory[j]-cx[j])*(cxhistory[j]-cx[j]))+((cyhistory[j]-cy[j])*(cyhistory[j]-cy[j])))
      totaldist2<-totaldist2+((cxhistory[j]-cx[j])*(cxhistory[j]-cx[j]))+((cyhistory[j]-cy[j])*(cyhistory[j]-cy[j]))
      #print("total dist2")
      #print(totaldist2)
      
    }
    #print("end")
    #print(totaldist2)
    
    if(totaldist2==0){
      break
    }
    dm[inde] <- totaldist2
    
    if(totaldist<0.001){
      break
    }
    
    
  }
  inde<-inde+1
  print("dm-")
  print(dm)
  plot(c(-3, 4), c(-1.5, 1.5), col="white")
  for(j in 1:k){
    lines(cl[[j]][,1], cl[[j]][,2], col=col[j])
  }
  
}
plot(1:12, seq(0.00, 0.11, 0.01), col="white")
lines(1:12, dm, col="red")