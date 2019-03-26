rm(list=ls())
library(png)
library(jpeg)
library(MASS)
library(mclust)
setwd("C:/Users/sinas/Desktop/data mining")
load("test.Rdata")
  load("train.Rdata")
attach(train)  

#mod1=Mclust(train[,2:5],G=3)
mod1.1=MclustDA(train[,2:5],train[,1])
summary(mod1.1)
pred=predict.MclustDA(mod1.1,test[,2:5])

cv=cvMclustDA(mod1.1,nfold = 10)
 #mod2=Mclust(test[,2:5]) 
mclustDplot(train[,2],parameters = mod2$parameters, z=mod2$z,
             truth = train$y,what = "classification")
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
load("nmx.Rd")
d=c(dim(nmx_1[[4]])[1],dim(nmx_1[[4]])[2])
my = matrix(mod2$classification,d[1],d[2])
my1 = matrix(pred$classification,d[1],d[2])
my2 = matrix(mda.pred,d[1],d[2])
my3 = matrix(mda.pred2,d[1],d[2])
plot.img(my,s=1, col=T)
plot.img(matrix(as.numeric(my1),dim(my1)[1],dim(my1)[2]),s=1, col=T)
error = sum(pred$classification!=test$y)/length(test$y)
########Mixture Discriminant Analysis######
library(mda)
mstars=mda.start(train[,-1],train[,1],start.method = "kmeans",tries=5)
mda=mda(y~x1+x2+x0, data=train)#,weights = mstars,subclasses=4 )
mda2=mda(y~x1+x2+x0, data=train,subclasses=2)
mda2=mda(y~x1+x2+x0, data=train,subclasses=10)
test1=test[,-1]
mda.pred=predict(mda, test[2:4],type = "class")
mda.pred2=predict(mda2, test[2:4],type = "class")
max(as.numeric(my1))
matrix(as.numeric(my1),dim(my1)[1],dim(my1)[2])
plot.img(matrix(as.numeric(my2),dim(my2)[1],dim(my2)[2]),s=1, col=T)
plot.img(matrix(as.numeric(my3),dim(my3)[1],dim(my3)[2]),s=1, col=T)
error = sum(mda.pred!=test$y)/length(test$y)
error = sum(mda.pred2!=test$y)/length(test$y)
