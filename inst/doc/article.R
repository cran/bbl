### R code from vignette source 'article.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: data
###################################################
titanic <- as.data.frame(Titanic)
head(titanic)


###################################################
### code chunk number 3: raw
###################################################
library(bbl)
titanic <- freq2raw(titanic, Freq='Freq')
head(titanic)
summary(titanic)


###################################################
### code chunk number 4: div
###################################################
set.seed(158)
nsample <- nrow(titanic)
flag <- rep(TRUE, nsample)
flag[sample(nsample, nsample/2)] <- FALSE
dtrain <- titanic[flag,]
dtest <- titanic[!flag,]


###################################################
### code chunk number 5: lr
###################################################
fit <- glm(Survived ~ ., family=binomial(), data=dtrain)
prl <- predict(fit, newdata=dtest)
pROC::roc(response=dtest$Survived, predictor=prl, direction='<')$auc


###################################################
### code chunk number 6: class
###################################################
model <- bbl(data=dtrain, y='Survived')
model


###################################################
### code chunk number 7: ps
###################################################
model <- train(model, method='pseudo', lambda=0)
model@h
head(model@J,n=1)


###################################################
### code chunk number 8: survival
###################################################
pr <- predict(model, newdata=dtest, logit=FALSE)
head(pr)
pROC::roc(response=dtest$Survived, predictor=pr[,2], direction='<')$auc


###################################################
### code chunk number 9: cv
###################################################
cv <- crossval(model, method='pseudo', lambda=10^seq(-6,-2,0.5),
               verbose=0)
cv


###################################################
### code chunk number 10: pr2
###################################################
lstar <- cv[cv$auc==max(cv$auc),]$lambda
model <- train(model, method='pseudo', lambda=lstar)
pr2 <- predict(model, newdata=dtest, progress.bar=FALSE)
yhat2 <- model@groups[apply(pr2,1,which.max)]
mean(dtest$Survived==yhat2)
pROC::roc(response=dtest$Survived, predictor=pr2[,2], direction='<')$auc


###################################################
### code chunk number 11: sim1
###################################################
predictors <- list()
m <- 5
L <- 3
for(i in 1:m) predictors[[i]] <- seq(0, L-1)
par <- randompar(predictors, dh=1, dJ=1, distr='unif')
names(par)


###################################################
### code chunk number 12: sample
###################################################
xi <- sample_xi(nsample=10000, predictors=predictors, h=par$h, J=par$J, 
                code_out=TRUE)
head(xi)


###################################################
### code chunk number 13: mle
###################################################
fit <- mlestimate(xi=xi, method='pseudo',lambda=0)
names(fit)


###################################################
### code chunk number 14: par
###################################################
oldpar <- par(mar = c(4,4,1,2),lwd=0.5,cex.axis=0.8,cex.lab=1.0,
              mgp=c(2.2,0.9,0),tck=-0.03)
range <- range(par$h, par$J, fit$h, fit$J)
plot(x=unlist(par$h), y=unlist(fit$h), bg='cornflowerblue', xlim=range, yli=range, pch=21,
     cex=0.8, xlab='True', ylab='Inferred', lwd=0.7, xaxt='n',yaxt='n',bty='n')
axis(side=1, at=seq(-1.5,1.5,0.5), lwd=0.5, las=1)
axis(side=2, at=seq(-1.5,1.5,0.5), lwd=0.5, las=1)
segments(x0=-1,x1=1,y0=-1,y1=1, lty=2, lwd=0.7)
points(x=unlist(par$J), y=unlist(fit$J), pch=24, bg='orange', cex=0.8, lwd=0.7)
legend(x=0.5,y=-0.5, legend=expression(italic(h), italic(J)), cex=0.8, pch=c(21,24), pt.bg=c('cornflowerblue',
       'orange'))
par(oldpar)


###################################################
### code chunk number 15: atgc
###################################################
set.seed(135)
n <- 1000
for(i in 1:m) predictors[[i]] <- c('a','c','g','t')
par <- xi <- list()
for(iy in 1:2){
  par[[iy]] <- randompar(predictors, h0=0.1*(iy-1), J0=0.1*(iy-1),
                         distr='unif')
  xi[[iy]] <- sample_xi(nsample=n, predictors=predictors, h=par[[iy]]$h, 
                        J=par[[iy]]$J)
}
dat <- cbind(rbind(xi[[1]],xi[[2]]), data.frame(y=c(rep('control',n),
                                                    rep('case',n))))
model <- bbl(data=dat, groups=c('control','case'))
model


###################################################
### code chunk number 16: cr-mf
###################################################
cv <- crossval(model, method='mf', eps=seq(0,1,0.1),verbose=0)
head(cv)


###################################################
### code chunk number 17: mf-par
###################################################
fit <- list()
eps <- c(0.2, 0.8, 1.0)
for(i in seq_along(eps))
  fit[[i]] <- train(model, method='mf', eps=eps[i], verbose=0)


###################################################
### code chunk number 18: cv
###################################################
oldpar <- par(mfrow=c(2,2),mar = c(4,4,2,2),lwd=0.5,cex.axis=0.8,
              cex.lab=0.9,mgp=c(2.2,0.8,0),tck=-0.03,las=1)
estar <- cv[cv[,2]==max(cv[,2]),1]
plot(x=cv$epsilon, y=cv$auc, type='b',xlab=expression(epsilon),ylab='AUC',lwd=0.7,cex=0.7,bty='n')
segments(x0=estar,x1=estar, y0=0, y1=cv[cv[,1]==estar,2], lty=2, lwd=0.5, col='red')
title(adj=0,cex.main=1.2,font=2,main='a')

range <- c(-1.5, 1.5)
for(i in 1:3){
  plot(x=c(unlist(par[[1]]$h), unlist(par[[2]]$h)),  y=unlist(fit[[i]]@h), 
       bg='cornflowerblue', xlim=range, ylim=range, pch=21,
       cex=0.7, xlab='True', ylab='Inferred', lwd=0.7, xaxt='n',yaxt='n',bty='n')
  axis(side=1, at=seq(-1.5,1.5,0.5), lwd=0.5, las=1)
  axis(side=2, at=seq(-1.5,1.5,0.5), lwd=0.5, las=1)
  segments(x0=-2,x1=2,y0=-2,y1=2, lty=2, lwd=0.7)
  points(x=c(unlist(par[[1]]$J),unlist(par[[2]]$J)), y=unlist(fit[[i]]@J), pch=24, bg='orange', 
         cex=0.7, lwd=0.7)
  if(i==1) legend(x=0.5,y=-0.5, legend=expression(italic(h), italic(J)), cex=0.8, pch=c(21,24), 
           pt.bg=c('cornflowerblue','orange'))
  title(adj=0,main=letters[i+1],cex.main=1.1,font=2)
  mtext(side=3,line=1.0,cex=0.8,bquote(epsilon==.(eps[i])),adj=0.5)
}
par(oldpar)


###################################################
### code chunk number 19: mnist
###################################################
dat <- read.csv(system.file('extdata/mnist_train.csv',package='bbl'))
dat[1:5,1:10]
mnist <- bbl(data=dat)
mnist


###################################################
### code chunk number 20: mnist2 (eval = FALSE)
###################################################
## cv <- crossval(mnist, method='mf', eps=0.1)


###################################################
### code chunk number 21: mnist3 (eval = FALSE)
###################################################
## mnist <- train(mnist, method='mf', eps=0.1)
## dtest <- read.csv(system.file('extdata/mnist_test.csv',package='bbl'))
## dtest <- dtest[,colnames(dtest) %in% colnames(mnist@data)]
## pr <- predict(mnist, newdata=dtest[,-1], progress.bar=FALSE)
## yhat <- colnames(pr)[apply(pr, 1, which.max)]
## mean(yhat==dtest$y)


###################################################
### code chunk number 22: jaspar
###################################################
seq <- read.fasta(system.file('extdata/MA0014.3.fasta',package='bbl'))
head(seq)
dim(seq)


###################################################
### code chunk number 23: jaspar2
###################################################
set.seed(561)
nsample <- NROW(seq)
m <- NCOL(seq)
nt <- c('A','C','G','T')
ctrl <- as.matrix(seq)
for(k in seq(nsample))
  ctrl[k, sample(m,3)] <- sample(nt, 3, replace=TRUE)
colnames(ctrl) <- 1:m
data <- rbind(data.frame(y=rep('Binding', nsample), seq), 
              data.frame(y=rep('Non-binding', nsample), ctrl))
data <- data[sample(NROW(data)), ]


###################################################
### code chunk number 24: jaspar3
###################################################
model <- bbl(data=data)
model
ps <- crossval(model, method='pseudo', lambda=10^seq(-1,-2,-0.2), verbose=0)
ps
mf <- crossval(model, method='mf', eps=seq(0.1,0.4,0.1),verbose=0)
mf


