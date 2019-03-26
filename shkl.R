library(klaR)
a<-as.matrix(vmx_1[[9]])
b<-as.matrix(vmx[[9]])
c<-as.matrix(vmy[[9]])
d<-as.data.frame(cbind(a,b,c))

u<- c(1:nrow(d))
u<-as.vector(sample(1:nrow(d),nrow(d)/30,replace=FALSE))
n<-length(u)
v<-as.data.frame(cbind(rep(0,n),rep(0,n),rep(0,n)))
for (i in 1:n){ 
  p= u[i]
  v[i,]= d[p,] }
z=v$V3
  
plot11<-ggplot(v,aes(V1,V2,col = c("red", "blue", "green")[z]))
plot(v$V1,v$V2,col = c("red", "blue","green"), pch = c(17, 15,16))
#######################################
train.x=NULL
train.x.1=NULL
train.x.2=NULL
train.x.3=NULL
train.yy= NULL
train.xx=NULL

for(i in c(1,2,3,5,6,7,8,9)){
  train.x=c(all.x,vmx[[i]])
  train.x.1=c(train.x.1,vmx_1[[i]])
  train.x.2=c(train.x.2,vmx_2[[i]])
  train.x.3=c(train.x.3,vmx_3[[i]])}


for(i in c(1,2,3,5,6,7,8,9)){
  train.yy=c(train.yy,vmy[[i]])}



test =  data.frame(x0 = vmx[[4]], x1 = vmx_1[[4]], x2 = vmx_2[[4]], x3 = vmx_3[[4]])
t = data.frame(y = factor(vmy[[4]]))
test=cbind(t,test)

train = data.frame(y = factor(train.yy), x0 = train.x, x1 = train.x.1, x2 = train.x.2, x3 = train.x.3 )
all= data.frame(y = factor(all.yy), x0 =all.x, x1 = all.x.1, x2 = all.x.2, x3 = all.x.3 )

fitlda=lda(y~x0+x1+x2, data=all)
fitqda=qda(y~x0+x1+x2, data=all)

fitlda2=lda(y~x0+x1+x2, data=all,CV=TRUE)
fitqda2=qda(y~x0+x1+x2, data=all,CV=TRUE)

fitglm=glm(y~x0+x1+x2, data=dat1, family='binomial')

tab1 <- table(all$y, fitlda2$class)
tab2 <- table(all$y, fitqda2$class)
confusion(all$y, fitlda2$class)
confusion(all$y, fitqda2$class)
##fit <- lda(x=x[,1:2],grouping=x[,3])
##A <- fit$scaling             # extract A matrix 
##sigmahatinv <- A%*%t(A)     # calculate sigma hat inverse
###priorhat <- fit$prior        # get prior hat probabilities
####muhat <- fit$means     

#ownldahyperplane <- function(sigmainv,mu1,mu2,prior1,prior2) {
 # J <- nrow(muhat)            # number of classes
#  b <- sigmainv%*%(mu1 - mu2)
#  c <- -(1/2)*t(mu1 + mu2)%*%sigmainv%*%(mu1 - mu2) + log(prior1/prior2) 
#  return(list(b=b,c=c))
#}

#ownlinearize <- function(sephyp) {
 # return(list(beta0=-sephyp$c/sephyp$b[2],         # line slope and intersect
  #            beta1=-sephyp$b[1]/sephyp$b[2]))
#}


#sephyp12 <- ownldahyperplane(sigmahatinv,muhat[1,],muhat[2,],# calculate dec. boundary 1-2
 #                           priorhat[1],priorhat[2])
#line12 <- ownlinearize(sephyp12)                      # get line for 1-2
#abline(line12$beta0,line12$beta1)


#sephyp23 <- ownldahyperplane(sigmahatinv,muhat[2,],muhat[3,],     # calculate dec. boundary 2-3
 #                            priorhat[2],priorhat[3])
#line23 <- ownlinearize(sephyp23)                                                       # get line for 2-3
#abline(line23$beta0,line23$beta1)

#sephyp13 <- ownldahyperplane(sigmahatinv,muhat[1,],muhat[3,],     # calculate dec. boundary 1-3
                             #priorhat[1],priorhat[3])
#line13 <- ownlinearize(sephyp13)                                                        # get line for 1-3
#abline(line13$beta0,line13$beta1)


###################
#partimat(x=dat1[,-1], grouping=as.factor(dat1[,1]), method="lda", 
 #        col.mean=1, image.colors = c("lightgrey","red","green"))
#
test =  data.frame(x0 = vmx[[4]], x1 = vmx_1[[4]], x2 = vmx_2[[4]], x3 = vmx_3[[4]])
t = data.frame(y = factor(vmy[[4]]))
test=cbind(t,test)
newdat = data.frame(x0 = vmx[[4]], x1 = vmx_1[[4]], x2 = vmx_2[[4]], x3 = vmx_3[[4]])
ldapredict=predict(fitlda, newdata=newdat)
qdapredict=predict(fitqda, newdata=newdat)
confusion(test$y, ldapredict$class)
confusion(test$y, qdapredict$class)

glmpredict=predict.glm(fitglm,test,type="response")
#partimat(x=test[,-5], grouping=as.factor(test[,5]), method="lda", 
        # col.mean=1, image.colors = c("lightgrey","red","green"))
##################3
confusion <- function(actual, predicted, names = NULL, printit = TRUE,
                       prior = NULL) {
   if (is.null(names))
     names <- levels(actual)
     tab <- table(actual, predicted)
     acctab <- t(apply(tab, 1, function(x) x/sum(x)))
     dimnames(acctab) <- list(Actual = names, "Predicted " = names)
     if (is.null(prior)) {
       relnum <- table(actual)
       prior <- relnum/sum(relnum)
       acc <- sum(tab[row(tab) == col(tab)])/sum(tab)
       }
     else {
       acc <- sum(prior * diag(acctab))
       names(prior) <- names
     }
     if (printit)
       print(round(c("Overall accuracy" = acc, "Prior frequency" = prior),
                     4))
     if (printit) {
       cat("\nConfusion matrix", "\n")
       print(round(acctab, 4))
       }
     invisible(acctab)
}

##############33
x.train<-all[,-1]
km<-kmeans(x, 15,nstart = 10)
v=as.data.frame(cbind(y=all.yy,x=km$cluster))
t=as.data.frame(table(v),head=TRUE)
x.test=test[,-1]
class=matrix(rep(0,15),ncol = 1)
for (i in 0:14){
  j=3*i+1
  c=as.data.frame(rbind(t[j,],t[j+1,],t[j+2,]))
  class[i+1]= c[which.max(c[,3]),1]-1
}
class
centers=km$centers
x.test=test[,-1]
y.hat<-NULL
dist<-NULL
for (i in 1:nrow(x.test)){
  dist<-NULL
  for (j in 1:15){
  dist[j]=dist(rbind(centers[j,],x.test[i,]),method = "euclidean")
  }
  y.hat[i]<- class[which.min(dist)]
}
my = matrix(y.hat,dim(nmx[[4]])[1],dim(nmx[[4]])[2])
plot.img(my,s=1, col=T)
error = sum(y.hat!=nmy[[4]])/length(nmy[[4]])

