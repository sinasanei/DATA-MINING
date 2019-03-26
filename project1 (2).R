rm(list=ls())
#install.packages("png")
#install.packages("jpeg")

library(png)
library(jpeg)

setwd("/Users/BSS/Documents/5. 2018.1 Spring/Stat 557/project1/good")

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

plot.img(nmx[[1]],s=5)
plot.img(nmy[[1]],s=5,col=T)
plot.img(nmx_1[[1]],s=5)
plot.img(nmx_2[[1]],s=5)
plot.img(nmx_3[[1]],s=5)

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
train.x =  NULL
train.x.1 =  NULL
train.x.2 =  NULL
train.x.3 =  NULL
train.y.1 =  NULL
train.y.2 =  NULL
train.y.3 =  NULL
ind = NULL
for(i in 1:8){
  train.x=c(train.x,vmx[[i]])
  train.x.1=c(train.x.1,vmx_1[[i]])
  train.x.2=c(train.x.2,vmx_2[[i]])
  train.x.3=c(train.x.3,vmx_3[[i]])
  train.y.1=c(train.y.1,vmy_1[[i]])
  train.y.2=c(train.y.2,vmy_2[[i]])
  train.y.3=c(train.y.3,vmy_3[[i]])
  ind = c(ind,rep(i,length(vmx[[i]])))
}

#-------------------------------------------------------------
# 1-1) Binary Logistic Regression with one covariate
dat1 = data.frame(y = factor(train.y.1), x = train.x)
dat2 = data.frame(y = factor(train.y.2), x = train.x)
dat3 = data.frame(y = factor(train.y.3), x = train.x)

fitglm_1=glm(y~x, data=dat1, family='binomial')
fitglm_2=glm(y~x, data=dat2, family='binomial')
fitglm_3=glm(y~x, data=dat3, family='binomial')
summary(fitglm_1)

k = 9       # test image
newdat = data.frame(x = vmx[[k]])
yglm_1=predict(fitglm_1, newdata=newdat, type='response')
yglm_2=predict(fitglm_2, newdata=newdat, type='response')
yglm_3=predict(fitglm_3, newdata=newdat, type='response')

#yglm_1 = as.integer(ifelse(yglm_1<mean(yglm_1),0,1))
#yglm_2 = as.integer(ifelse(yglm_2<mean(yglm_2),0,1))
#yglm_3 = as.integer(ifelse(yglm_2<mean(yglm_2),0,1))


yglm = apply(cbind(yglm_1,yglm_2,yglm_3),1,which.max)-1
table(yglm)
table(nmy[[k]])
my = matrix(yglm,dim(nmx[[k]])[1],dim(nmx[[k]])[2])
error = sum(yglm!=nmy[[k]])/length(nmy[[k]])

plot.img(my,s=5,col=T)
plot.img(nmy[[k]],s=5,col=T)
plot.img(nmx[[k]])


#-------------------------------------------------------------
# 1-2) Binary Logistic Regression with more covariates
dat1 = data.frame(y = factor(train.y.1), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
dat2 = data.frame(y = factor(train.y.2), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
dat3 = data.frame(y = factor(train.y.3), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )

fitglm2_1=glm(y~x0+x1+x2, data=dat1, family='binomial')
fitglm2_2=glm(y~x0+x1+x2, data=dat2, family='binomial')
fitglm2_3=glm(y~x0+x1+x2, data=dat3, family='binomial')
summary(fitglm_1)

newdat = data.frame(x0 = vmx[[k]], x1 = vmx_1[[k]], x2 = vmx_2[[k]], x3 = vmx_3[[k]])
yglm_1=predict(fitglm_1, newdata=newdat, type='response')
yglm_2=predict(fitglm_2, newdata=newdat, type='response')
yglm_3=predict(fitglm_3, newdata=newdat, type='response')

yglm = apply(cbind(yglm_1,yglm_2,yglm_3),1,which.max)-1
table(yglm)
table(nmy[[k]])
my = matrix(yglm,dim(nmx[[k]])[1],dim(nmx[[k]])[2])
error = sum(yglm!=nmy[[k]])/length(nmy[[k]])

plot.img(my,s=3, col=T)
plot.img(nmy[[k]], s=3, col=T)
plot.img(nmx[[k]], s=3)

#-------------------------------------------------------------
# 3) LDA
library(MASS)
fitlda1=lda(factor(y)~x0+x1+x2, data=dat1)
fitlda2=lda(factor(y)~x0+x1+x2, data=dat2)
fitlda3=lda(factor(y)~x0+x1+x2, data=dat3)
summary(fitlda1)

ylda1=predict(fitlda1, newdata=newdat)
ylda2=predict(fitlda2, newdata=newdat)
ylda3=predict(fitlda3, newdata=newdat)

ylda = apply(cbind(ylda1$posterior[,2],ylda2$posterior[,2],ylda3$posterior[,2]),1,which.max)-1
table(ylda)
table(nmy[[k]])
my = matrix(ylda,dim(nmx[[k]])[1],dim(nmx[[k]])[2])
error = sum(ylda!=nmy[[k]])/length(nmy[[k]])

plot.img(my,s=3, col=T)
plot.img(nmy[[k]], s=3, col=T)
plot.img(nmx[[k]], s=3)

#-------------------------------------------------------------
# 4) QDA
library(MASS)
fitqda1=qda(factor(y)~x0+x1+x2, data=dat1)
fitqda2=qda(factor(y)~x0+x1+x2, data=dat2)
fitqda3=qda(factor(y)~x0+x1+x2, data=dat3)
summary(fitqda)

yqda1=predict(fitqda1, newdata=newdat)
yqda2=predict(fitqda2, newdata=newdat)
yqda3=predict(fitqda3, newdata=newdat)

yqda = apply(cbind(yqda1$posterior[,2],yqda2$posterior[,2],yqda3$posterior[,2]),1,which.max)-1
table(yqda)
table(nmy[[k]])
my = matrix(yqda,dim(nmx[[k]])[1],dim(nmx[[k]])[2])
error = sum(yqda!=nmy[[k]])/length(nmy[[k]])

plot.img(my,s=3, col=T)
plot.img(nmy[[k]], s=3, col=T)
plot.img(nmx[[k]], s=3)

#-------------------------------------------------------------
# 5) GMME
library(lme4)
dat1 = data.frame(id = ind, y = factor(train.y.1), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
dat2 = data.frame(id = ind, y = factor(train.y.2), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
dat3 = data.frame(id = ind, y = factor(train.y.3), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )

fitglmer_1=glmer(y~x0+x1+x2+(1|id), data=dat1, family='binomial')
fitglmer_2=glmer(y~x0+x1+x2+(1|id), data=dat2, family='binomial')
fitglmer_3=glmer(y~x0+x1+x2+(1|id), data=dat3, family='binomial')
summary(fitglm_1)
save(fitglmer_1,fitglmer_2,fitglmer_3, file="fitglmer")

newdat = data.frame(id = rep(1,length(vmx[[k]])), x0 = vmx[[k]], x1 = vmx_1[[k]], x2 = vmx_2[[k]])#, x3 = vmx_3[[k]])
yglmer_1=predict(fitglmer_1, newdata=newdat, type='response')
yglmer_2=predict(fitglmer_2, newdata=newdat, type='response')
yglmer_3=predict(fitglmer_3, newdata=newdat, type='response')

yglmer = apply(cbind(yglmer_1,yglmer_2,yglmer_3),1,which.max)-1
table(yglmer)
table(nmy[[k]])
my = matrix(yglmer,dim(nmx[[k]])[1],dim(nmx[[k]])[2])
error = sum(yglmer!=nmy[[k]])/length(nmy[[k]])

plot.img(my,s=3, col=T)
plot.img(nmy[[k]], s=3, col=T)
plot.img(nmx[[k]], s=3)
