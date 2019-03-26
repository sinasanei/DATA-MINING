rm(list=ls())
#install.packages("png")
#install.packages("jpeg")

library(png)
library(jpeg)

setwd("C:/Users/sinas/Desktop/data mining")

file_no = c(1,2,3,4,5,14,15,16,20,21) #,23,27,28,29,33,34,35,36,42,55,58,65,67,68,69)
i=1
j=1
for(i in 1:10){
  no = file_no[i]
  x = readJPEG(paste("image (",no,").jpg",sep=""))
  y = readPNG(paste(no,"_m.png",sep=""))
  
  xxx = x[,,1]
  yy = matrix(0,dim(y)[1]*dim(y)[2])
  yy[which(y[,,1]==1)] = 1
  yy[which(y[,,3]==1)] = 2
  yyy = matrix(yy,dim(y)[1],dim(y)[2])
  
  nam_x <- paste("mx", i, sep = "")
  nam_y <- paste("my", i, sep = "")
  assign(nam_x, xxx)
  assign(nam_y, yyy)
  
  nam_x <- paste("vx", i, sep = "")
  nam_y <- paste("vy", i, sep = "")
  assign(nam_x, matrix(xxx,dim(x)[1]*dim(x)[2])) #stack each column
  assign(nam_y, matrix(yyy,dim(y)[1]*dim(y)[2])) #stack each column
  
  rm(x,y,yy,xxx,yyy)
}
hist(vx1)
hist(vy1)
#save.image(file="image.Rd")
load("image.Rd")
##############################################################
# check the image
plot.img <- function(samp.x, s=10, col=F){
  #s=5  # definition of image
  #samp.x = nmx_2[[1]]                 #image x
  samp.bw = (samp.x-min(samp.x))/(max(samp.x)-min(samp.x)) #normalize
  samp.co = samp.x
  
  ys = floor(dim(samp.x)[1]/s)
  xs = floor(dim(samp.x)[2]/s)
  plot(NULL,xlim=c(1,xs),ylim=c(-ys,-1))
  
  for(i in 1:xs){
    for(j in 1:ys){
      if(col==F) points(i,-j,col=grey(samp.bw[j*s,i*s]),pch=19)  # image x
      if(col==T) points(i,-j,col=samp.co[j*s,i*s],pch=19)  # image x
      
    }
  }
}

plot.range <- function(x,y){
  plot.default(x=x,y=y,pch=19,cex=0.8,col=y+1,xlab="Gray value of each pixel",ylab="Predicted class",xlim=c(0,1),ylim=c(0,3))
  legend(0, 3, legend=c("Background","Picture","Text"),
         col=c("black","red","green"), pch=19, cex=0.8,
         horiz=T,bty="n")
}

x=newdat
y=yglm2
plot.scatter <- function(x,y){
  xx=x[,2];xy=x[,3]
  plot(xx, xy, pch=19,cex=0.8,col=y+1, xlab="Variance of 21x21 pixel matrix",ylab="Average of 21x21 pixel matrix")
  legend(min(xx),max(xy), 1, legend=c("Background","Picture","Text"),
         col=c("black","red","green"), pch=19, cex=0.8,
         horiz=T,bty="n")
}
#hist(samp.x)
##############################################################
# shrink the size

nmx = list()
nmy = list()
vmx = list()
vmy = list()
for(i in 1:10){
  s=10
  
  mx = eval(parse(text=paste("mx",i,sep="")))
  my = eval(parse(text=paste("my",i,sep="")))
  
  dimv = dim(mx)[1]
  dimh = dim(mx)[2]
  ndimv = floor(dimv/s)
  ndimh = floor(dimh/s)
  
  lv = s*(1:ndimv)
  lh = s*(1:ndimh)
  
  xxx = mx[lv,lh]
  yyy = my[lv,lh]
  
  nmx[[i]] = xxx
  nmy[[i]] = yyy
  #nam_x <- paste("nmx", i, sep = "")
  #nam_y <- paste("nmy", i, sep = "")
  #assign(nam_x, xxx)
  #assign(nam_y, yyy)
  
  vmx[[i]] = matrix(xxx,ndimv*ndimh)
  vmy[[i]] = matrix(yyy,ndimv*ndimh)
  #nam_x <- paste("nvx", i, sep = "")
  #nam_y <- paste("nvy", i, sep = "")
  #assign(nam_x, matrix(xxx,ndimv*ndimh)) #stack each column
  #assign(nam_y, matrix(yyy,ndimv*ndimh)) #stack each column
  
  rm(mx,my,lv,lh,xxx,yyy)
}

jpeg(file = "notshrunk.jpg")
plot.img(mx4,s=10)
dev.off()
##############################################################
# create variables

r = 10 #radius

nmx_1 = list()
nmx_2 = list()
nmx_3 = list()

vmx_1 = list()
vmx_2 = list()
vmx_3 = list()
for(i in 1:10){
  mx = nmx[[i]]
  
  dimv = dim(mx)[1]
  dimh = dim(mx)[2]
  
  x1 = matrix(NA,dimv,dimh)
  x2 = matrix(NA,dimv,dimh)
  x3 = matrix(NA,dimv,dimh)
  
  for(v in 1:dimv){
    for(h in 1:dimh){
      minv = max(1,v-r)
      maxv = min(dimv,v+r)
      minh = max(1,h-r)
      maxh = min(dimh,h+r)
      
      x1[v,h] = var(c(mx[minv:maxv,minh:maxh]))
      x2[v,h] = mean(mx[minv:maxv,minh:maxh])
    }
  }
  x3 = x2-mx
  
  nmx_1[[i]] = x1
  nmx_2[[i]] = x2
  nmx_3[[i]] = x3
  
  vmx_1[[i]] = matrix(x1,dimv*dimh)
  vmx_2[[i]] = matrix(x2,dimv*dimh)
  vmx_3[[i]] = matrix(x3,dimv*dimh)
  print(i)
}
#save(nmx_1,nmx_2,nmx_3, file="nmx.Rd")
#save(vmx_1,vmx_2,vmx_3, file="vmx.Rd")
load("nmx.Rd")
load("vmx.Rd")

dim(x2)
dim(mx)
dim(x3)

plot.img(nmx[[4]],s=3)
plot.img(nmy[[4]],s=3,col=T)
plot.img(nmx_1[[4]],s=3)
plot.img(nmx_2[[4]],s=3)
plot.img(nmx_3[[4]],s=3)

##############################################################
# analysis
i=1
vmy_1 = list(); vmy_2 = list(); vmy_3 = list()
for(i in 1:10){
  my = vmy[[i]]
  n = length(my)
  
  my_1 = rep(0,n)
  my_2 = rep(0,n)
  my_3 = rep(0,n)
  my_1[which(my==0)] = 1
  my_2[which(my==1)] = 1
  my_3[which(my==2)] = 1
  
  vmy_1[[i]] = my_1
  vmy_2[[i]] = my_2
  vmy_3[[i]] = my_3
  rm(my_1,my_2,my_3)
}
table(my)
#-------------------------------------------------------------
#######################################
train.x=NULL
train.x.1=NULL
train.x.2=NULL
train.x.3=NULL
train.yy= NULL
train.xx=NULL

for(i in c(1,2,3,5,6,7,8,9)){
  train.x=c(train.x,vmx[[i]])
  train.x.1=c(train.x.1,vmx_1[[i]])
  train.x.2=c(train.x.2,vmx_2[[i]])
  train.x.3=c(train.x.3,vmx_3[[i]])}


for(i in c(1,2,3,5,6,7,8,9)){
  train.yy=c(train.yy,vmy[[i]])}



test =  data.frame(x0 = vmx[[4]], x1 = vmx_1[[4]], x2 = vmx_2[[4]], x3 = vmx_3[[4]])
t = data.frame(y = factor(vmy[[4]]))
test=cbind(t,test)

train = data.frame(y = factor(train.yy), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
## Analysis k-means
set.seed(22)
x.train<-train[,-1]
error=rep(0,19)
my=list()
for(m in 2:20){
km<-kmeans(x.train, m,nstart = 20)
v=as.data.frame(cbind(y=as.numeric(train.yy),x=km$cluster))
t=as.data.frame(table(v),head=TRUE)# counting classes in each prototype
t$y=as.numeric(t$y)
x.test=test[,-1]
class=matrix(rep(0,m),ncol = 1)
for (k in 0:(m-1)){  ## asigning class labels to prototypes
  j=3*k+1
  c=as.data.frame(rbind(t[j,],t[j+1,],t[j+2,]))
  class[k+1]= c[which.max(c[,3]),1]-1   
}
class
centers=km$centers
x.test=test[,-1]
y.hat<-NULL
dist<-NULL  ### calculating distance of test features from centers(prototypes)
for (i in 1:nrow(x.test)){
  dist<-NULL
  for (j in 1:m){
    dist[j]=dist(rbind(centers[j,],x.test[i,]),method = "euclidean")
  }
  y.hat[i]<- class[which.min(dist)]
}
my[[m]] = matrix(y.hat,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
error[m] = sum(y.hat!=nmy[[4]])/length(nmy[[4]])
}
#save(my,file="result.Rdata")
#save(error,file="errorkm.Rdata")
load("result.Rdata")
load("errorkm.Rdata")
plot.img(my[[11]],s=1, col=T)

##### KNN
library(class)
y.test<- test[,1]
y.train<- train[,1]
error.knn=rep(0,11)
knn.table<-list()
my.knn<-list()
tunning.param<-c(1,3,5,10,15,17,20,25,35,50,100)
set.seed (22)
for( i in 1:11){
    knn.fit =knn ( x.train,x.test, y.train ,k=tunning.param[i])
    knn.table[[i]]<-table (knn.fit , y.test)
    c<-as.data.frame(table(knn.fit , y.test))
    error.knn[i]<-sum(c$Freq[which(c$y.test!=c$knn.fit)]/sum(c$Freq))
    my.knn[[i]]=matrix(knn.fit,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
}
#knn.table[11]
#save(my.knn,file="resultknn.Rdata")
#save(error.knn,file="errorknn.Rdata")
#my.knn2 = matrix(my.knn2,d[1],d[2])
#plot.img(my.knn2,s=1, col=T)
load("resultknn.Rdata")
load("errorknn.Rdata")
#-------------------------------
###########Principal component#########
prin.comp<- prcomp(x.test, scale=TRUE)
pc<-as.matrix(cbind(prin.comp$x[,1],prin.comp$x[,2]))
ukm<-kmeans(pc,10, nstart = 5)
plot(pc, col =( ukm$cluster +1) , main ="K- Means Clustering
Results with K=10", xlab ="", ylab ="", pch =20, cex =2)
ukm1<-kmeans(x.test,10,nstart=20)
clust1 = matrix(ukm1$cluster,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
plot.img(clust1,s=1, col=T)
#######_________________K-Center
set.seed(22)
center=list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
a<-sample(1:nrow(x.test),1)
center[[1]]<-x.test[a,]
data<-x.test
dist=NA
class<-NA
for( i in 1:nrow(data)){
dist[i]<-dist(rbind(center[[1]],data[i,]),method = "euclidean")
}
for( j in 2:11){
  center[[j]]<-data[which.max(dist),]
  for( k in 1:nrow(data)){
    d<-NA
    for(n in 1:j){
      d[n]<-dist(rbind(center[[n]],data[k,]),method = "euclidean")}
    dist[k]<-min(d)
    class[k]<- which.min(d)
  }
}
save(center,file="k-center.Rdata")
save(class,file="k-center-class.Rdata")
#####K-mean with K-center initialization
save(x.test,file="x.test.Rdata")
centers=as.numeric(center[[1]])
save(y.test,file="y.test.Rdata")
for (i in 2:11){
  centers<- rbind(centers,center[[i]])
}

km.kcenter<-kmeans(x.test, centers = centers,nstart = 1)
my.kc = matrix(classs,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
plot.img(my.kc,s=1, col=T)

v=as.data.frame(cbind(y=as.numeric(y.test),x=km.kcenter$cluster))
t=as.data.frame(table(v),head=TRUE)# counting classes in each prototype
t$y=as.numeric(t$y)
class.kmean.kcenter=matrix(rep(0,length(y.test)),ncol = 1)
for (k in 0:10){  ## asigning class labels to prototypes
  j=3*k+1
  c=as.data.frame(rbind(t[j,],t[j+1,],t[j+2,]))
  class.kmean.kcenter[which(km.kcenter$cluster==(k+1))]= c[which.max(c[,3]),1]-1   
}
my.kc.1 = matrix(class.kmean.kcenter,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
plot.img(my.kc.1,s=1, col=T)
error = sum(class.kmean.kcenter!=y.test)/length(y.test)
error
