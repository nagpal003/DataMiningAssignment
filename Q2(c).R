library(EMCluster)
d<-read.table("C:/Users/deepak14035/Desktop/IRIS.csv",sep=",")
d$V9<-NULL
x<-prcomp(d)
s <- as.data.frame(x$x)
data<-s[,c(1,2)]
ret.em<-init.EM(data, nclass = 4, lab=lab, EMC = .EMC, method = c("em.EM", "Rnd.EM"))
ret.Rnd <- init.EM(data, nclass = 4, method = "Rnd.EM", EMC = .EMC.Rnd)
emobj <- simple.init(data, nclass = 4)
#plotem(ret.em, data, main = "em")
plotem(ret.Rnd, data, main = "em")

#ret.em
#ret.Rnd
#Mu
#emobj
#data
#demo(allinit,'EMCluster', ask = F)
#demo(allinit_ss,'EMCluster')