rm(list=ls())
#install.packages("png")
#install.packages("jpeg")

library(png)
library(jpeg)

setwd("/Users/BSS/Documents/5. 2018.1 Spring/Stat 557/project1/good")
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

##############################################################
# check the image

s=50  # definition of image
samp.x = mx2 #image x
samp.y = my2 #image y

ys = floor(dim(samp.x)[1]/s)
xs = floor(dim(samp.x)[2]/s)
plot(NULL,xlim=c(1,xs),ylim=c(-ys,-1))

for(i in 1:xs){
  for(j in 1:ys){
    points(i,-j,col=grey(samp.x[j*s,i*s]))  # image x
    #points(i,-j,col=samp.y[j*s,i*s])       # image y
  }
}

#################################
dim<-c(dim(mx1),dim(mx2),dim(mx3),dim(mx4),dim(mx5),dim(mx6),dim(mx7),dim(mx8),dim(mx9),dim(mx10))
for(k in 1:10){
  nam_x <- paste("nx", k, sep = "")
  nam_y <- paste("ox", k, sep = "")
  assign(nam_x,matrix(NA,dim[2*k-1],dim[2*k]))
  assign(nam_y,matrix(NA,dim[2*k-1],dim[2*k]))}
  for (i in 3:dim[2*k-1]-3){
    for(j in 3:dim[2*k]-3){ 
      nx1[i,j]=
      (eval(parse(text=paste("mx",k,sep="")))[i-1,j]
      +eval(parse(text=paste("mx",k,sep="")))[i,j-1]
      +eval(parse(text=paste("mx",k,sep="")))[i+1,j]
      +eval(parse(text=paste("mx",k,sep="")))[i,j+1]
      +eval(parse(text=paste("mx",k,sep="")))[i-1,j-1]
      +eval(parse(text=paste("mx",k,sep="")))[i+1,j-1]
      +eval(parse(text=paste("mx",k,sep="")))[i-1,j+1]
      +eval(parse(text=paste("mx",k,sep="")))[i+1,j+1])/8
  
    }}
  
##############################
y<-vy1
for (i in 2:9){y<-rbind(y,eval(parse(text=paste("vy",i,sep=""))))}
x<-vx1

for (i in 2:9){x<-rbind(x,eval(parse(text=paste("vx",i,sep=""))))}
training=as.data.frame(cbind(x,y))
colnames(training)=c("x","y")
head(training)
test<- as.data.frame(cbind(vx10,vy10))
colnames(test)=c("x","y")
##########################
##Fit the model
# 1) Binary Logistic Regression
fitglm=glm(factor(y)~x, data=training, family='binomial')
summary(fitglm)
yglm=predict(fitglm, newdata=test, type='response')
summary(yglm)
nature=which(yglm<=(mean(yglm)+0.07))
yhat=yglm
yhat[nature]=0.5
yhat[-nature]=1
jpeg("1BLR.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat))
dev.off()
mean((test$y==yhat))