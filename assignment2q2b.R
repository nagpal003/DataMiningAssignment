d<-read.table("C:/Users/Aashish/Downloads/IRIS.csv",sep=",")
d$V9<-NULL
x<-prcomp(d)
s <- as.data.frame(x$x)
data<-s[,c(1,2)]
data
rando<-sample(1:100,12)#generate 12 random indices
rando
col=c("red","yellow","skyblue4","purple","pink","black","brown","green","grey","lightskyblue","darkgoldenrod","chocolate")
cx<-c(data[rando[1],1], data[rando[2],1], data[rando[3],1], data[rando[4],1])
cy<-c(data[rando[1],2], data[rando[2],2], data[rando[3],2], data[rando[4],2])
i=2
while(i<=12)
{
	cx<-c(cx,c(data[rando[i],1]))
	cy<-c(cy,c(data[rando[i],2]))
	i=i+1
}

cxhistory<-runif(12,min(data[,1]), max(data[,1]))
cyhistory<-runif(12,min(data[,1]), max(data[,1]))
cl<-list()
rows<-nrow(data)
while(TRUE){
	i=1
	while(i<=12)
	{
		cl[[i]]<-matrix( nrow = 100, ncol = 2, byrow = TRUE)
		i=i+1
	}
	filled<-c(0,0,0,0,0,0,0,0,0,0,0,0)
	for(i in 1:rows){
		min<- 9999
		minx<- -1
		miny<- -1
		clust<- 1
		for(j in 1:12){
			  dist<-sqrt(((data[i,1]-cx[j])*(data[i,1]-cx[j]))+((data[i,2]-cy[j])*(data[i,2]-cy[j])))
			  if(min>dist){
				min<-dist
				clust<- j
			  }
			  
			  
		}
		filled[clust]<-filled[clust]+1
		cl[[clust]][filled[1],1]<-data[i,1]
		cl[[clust]][filled[1],2]<-data[i,2]
		
		
		#print(min)
	  }
	  for(i in 1:12)
	  {
		 cxhistory[i]<-cx[i]
		 cyhistory[i]<-cy[i]
		 cx[i]<-mean(cl[[i]][,1], na.rm = TRUE)
		 cy[i]<-mean(cl[[i]][,2], na.rm = TRUE)
	  }
	  
	  totaldist<-0
	  for(j in 1:12){
		totaldist<-totaldist+sqrt(((cxhistory[j]-cx[j])*(cxhistory[j]-cx[j]))+((cyhistory[j]-cy[j])*(cyhistory[j]-cy[j])))
		
	  }
	  if(totaldist<0.001){
		break
	  }
	  plot(c(-3, 4), c(-1.5, 1.5), col="white")
	  for(i in 1:12)
	  {
		 lines(cl[[i]][,1], cl[[i]][,2], col=col[i])
	  }
	  
}
