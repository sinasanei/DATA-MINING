install.packages("pixmap")
library(MASS)
library(pixmap)
## Read Original Image(x) and Classified Image(y)
cy1=read.pnm("1.cls.pgm")
cx1=read.pnm("1.pgm")
cy2=read.pnm("3.cls.pgm")
cx2=read.pnm("3.pgm")
cy3=read.pnm("4.cls.pgm")
cx3=read.pnm("4.pgm")
cy4=read.pnm("7.cls.pgm")
cx4=read.pnm("7.pgm")
cy5=read.pnm("8.cls.pgm")
cx5=read.pnm("8.pgm")
cy6=read.pnm("11.cls.pgm")
cx6=read.pnm("11.pgm")
## Make x,y into vector data
for(i in 1:6)
{
  nam_x <- paste("x", i, sep = "")
  nam_y <- paste("y", i, sep = "")
  assign(nam_x, getChannels(eval(parse(text=paste("cx",i,sep="")))))
  assign(nam_y, getChannels(eval(parse(text=paste("cy",i,sep="")))))
}
for(i in 1:6)
{
  nam_x <- paste("zx", i, sep = "")
  nam_y <- paste("zy", i, sep = "")
  assign(nam_x, matrix(eval(parse(text=paste("x",i,sep=""))),512*512,1))
  assign(nam_y, matrix(eval(parse(text=paste("y",i,sep=""))),512*512,1))
}
##Plot Images
xy=matrix("NA",512*512,2)
for(i in 1:512){
  for(j in 1:512){
    xy[i+j*512-512,2]=as.numeric(513-i)
    xy[i+j*512-512,1]=as.numeric(j)
  }
}
jpg_nam=matrix(NA,6,2)
for(i in 1:6){
  jpg_nam[i,1] <- paste("rplot_x",i,".jpg",sep="")
  jpg_nam[i,2] <- paste("rplot_y",i,".jpg",sep="")
}
i=4
j=1
par(mar=c(0,0,0,0))
for(j in 1:2){
  for(i in 1:6){
    if(j == 1) { assign("val",eval(parse(text=paste("zx",i,sep="")))) }
    else { assign("val",eval(parse(text=paste("zy",i,sep="")))) }
    jpeg(jpg_nam[i,j])
    plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
    points(xy[,1],xy[,2],pch='.',col=grey(val))
    dev.off()
  }
}
##Devide training and test data (test set is chosen as photo 4)
set.seed(6)
k=sample(1:6,1)
k
test=as.data.frame(cbind(zx4,zy4))
colnames(test)=c("x","y")
training=as.data.frame(cbind(rbind(zx1,zx2,zx3,zx5,zx6),rbind(zy1,zy2,zy3,zy5,zy6)))
colnames(training)=c("x","y")
## Calculate the similarity of photos based on the grey scale pattern of each photo.
# Calculate average grey scale of 8 neighbor pixels
# for training set
p1=training[1:(512*512),]
p2=training[(512*512*1+1):(512*512*2),]
p3=training[(512*512*2+1):(512*512*3),]
p4=training[(512*512*3+1):(512*512*4),]
p5=training[(512*512*4+1):(512*512*5),]
for(p in 2:3)
{
  assign("data", eval(parse(text=paste("p",p,sep=""))))
  data=cbind(data,NA)
  d=1
  k=((2*d+1)^2-(2*d-1)^2)
  for(i in 2:511){
    for(j in 2:511){
      data[i+512*j,3]=1/k*(data[(i-d)+512*j,1]+data[(i+d)+512*j,1]
                           +data[i+512*(j-d),1]+data[i+512*(j+d),1]
                           +data[(i-d)+512*(j-d),1]+data[(i+d)+512*(j+d),1]
                           +data[(i+d)+512*(j-d),1]+data[(i-d)+512*(j+d),1])
    }
  }
  data[which(is.na(data[,3])),3]=data[which(is.na(data[,3])),1]
  colnames(data)=c("x","y","x2")
  head(data)
  nam <- paste("q", p, sep = "")
  assign(nam, data)
}
saveRDS(q1,"q1.data")
saveRDS(q2,"q2.data")
saveRDS(q3,"q3.data")
saveRDS(q4,"q4.data")
saveRDS(q5,"q5.data")
# for test set
data=test
data=cbind(data,NA)
d=1
k=((2*d+1)^2-(2*d-1)^2)
for(i in 2:511){
  for(j in 2:511){
    data[i+512*j,3]=1/k*(data[(i-d)+512*j,1]+data[(i+d)+512*j,1]
                         +data[i+512*(j-d),1]+data[i+512*(j+d),1]
                         +data[(i-d)+512*(j-d),1]+data[(i+d)+512*(j+d),1]
                         +data[(i+d)+512*(j-d),1]+data[(i-d)+512*(j+d),1])
  }
}
data[which(is.na(data[,3])),3]=data[which(is.na(data[,3])),1]
colnames(data)=c("x","y","x2")
head(data)
tt.data=data
saveRDS(tt.data,"tt.data")
# Similarity of photos
mean((q1$x-q1$x2)^2)
mean((q2$x-q2$x2)^2)
mean((q3$x-q3$x2)^2)
mean((q4$x-q4$x2)^2)
mean((q5$x-q5$x2)^2)
mean((tt$x-tt$x2)^2)
## Choose best training set (as photo 1)
training2=as.data.frame(cbind(zx1,zy1))
colnames(training2)=c("x","y")
head(training2)
##Fit the model
# 1) Binary Logistic Regression
fitglm=glm(factor(y)~x, data=training2, family='binomial')
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
# 2) Linear Discriminant Analysis
fitlda=lda(factor(y)~x, data=training2, prior=c(0.58,0.42))
summary(fitlda)
ylda=predict(fitlda,newdata=test)
head(ylda$posterior)
ylda1=ylda$posterior
nature=which(ylda1[,1]>=ylda1[,2])
yhat=ylda1[,1]
yhat[nature]=0.5
yhat[-nature]=1
summary(yhat)
jpeg("2LDA.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat))
dev.off()
mean((test$y==yhat))
jpeg("2LDA_assumption1.jpg")
plot(fitlda)
dev.off()
jpeg("2LDA_assumption2.jpg")
par(mfrow=c(1,2))
qqnorm(training2$x[which(training2$y==0.5)])
qqnorm(training2$x[which(training2$y==1)])
dev.off()
g1=training2$x[which(training2$y==0.5)]
g2=training2$x[which(training2$y==1)]
mean((g1-mean(g1))^2)
mean((g2-mean(g2))^2)
par(mfrow=c(1,1))
# 3) Quadratic Discriminant Analysis
fitqda=qda(factor(y)~x, data=training2, prior=c(0.61,0.39))
summary(fitqda)
yqda=predict(fitqda,newdata=test)
head(yqda$posterior)
yqda1=yqda$posterior
nature=which(yqda1[,1]>=yqda1[,2])
yhat=yqda1[,1]
yhat[nature]=0.5
yhat[-nature]=1
summary(yhat)
jpeg("3QDA.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat))
dev.off()
mean((test$y==yhat))
##Fit the model with spatial information
# 1) Binary Logistic Regression
fitglm=glm(factor(y)~x+x2, data=q1, family='binomial')
summary(fitglm)
yglm=predict(fitglm, newdata=tt, type='response')
summary(yglm)
nature=which(yglm<=(mean(yglm)+0.07))
yhat1=yglm
yhat1[nature]=0.5
yhat1[-nature]=1
jpeg("9.1BLR.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat1))
dev.off()
mean((test$y==yhat1))
# 2) Linear Discriminant Analysis
fitlda=lda(factor(y)~x+x2, data=q1, prior=c(0.58,0.42))
summary(fitlda)
ylda=predict(fitlda,newdata=tt)
head(ylda$posterior)
ylda1=ylda$posterior
nature=which(ylda1[,1]>=ylda1[,2])
yhat2=ylda1[,1]
yhat2[nature]=0.5
yhat2[-nature]=1
summary(yhat)
jpeg("9.2LDA.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat2))
dev.off()
mean((test$y==yhat2))
# 3) Quadratic Discriminant Analysis
fitqda=qda(factor(y)~x+x2, data=q1, prior=c(0.61,0.39))
summary(fitqda)
yqda=predict(fitqda,newdata=tt)
head(yqda$posterior)
yqda1=yqda$posterior
nature=which(yqda1[,1]>=yqda1[,2])
yhat3=yqda1[,1]
yhat3[nature]=0.5
yhat3[-nature]=1
summary(yhat3)
jpeg("9.3QDA.jpg")
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat3))
dev.off()
mean((test$y==yhat3))
jpeg("9.with.spatial.cov.jpg")
par(mfrow=c(1,3))
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat1))
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat2))
plot(NA,xlim=c(1,512),ylim=c(1,512),axes=FALSE,ylab="",xlab="")
points(xy[,1],xy[,2],pch='.',col=grey(yhat3))
dev.off()