yqda11=qda(factor(y)~x0+x1+x2, data=dat1,CV=TRUE)
yqda22=qda(factor(y)~x0+x1+x2, data=dat2,CV=TRUE)
yqda33=qda(factor(y)~x0+x1+x2, data=dat3,CV=TRUE)
yqdaa = apply(cbind(yqda11$posterior[,2],yqda22$posterior[,2],yqda33$posterior[,2]),1,which.max)-1
xtable(confusion(all$y, yqdaa))


ylda11=lda(factor(y)~x0+x1+x2, data=dat1,CV=TRUE)
ylda22=lda(factor(y)~x0+x1+x2, data=dat2,CV=TRUE)
ylda33=lda(factor(y)~x0+x1+x2, data=dat3,CV=TRUE)
yqdaa = apply(cbind(ylda11$posterior[,2],ylda22$posterior[,2],ylda33$posterior[,2]),1,which.max)-1
xtable(confusion(all$y, yqdaa))
