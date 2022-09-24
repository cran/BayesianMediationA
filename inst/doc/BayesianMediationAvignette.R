## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error=TRUE,
  warning=FALSE
)

## ---- include=FALSE-----------------------------------------------------------
library(BayesianMediationA)
#source('O:/My Documents/My Research/Research/Multilevel mediation analysis/mlma package/current version/R/mlma.r')

## -----------------------------------------------------------------------------
#use a simulation
set.seed(1)
N=100

alpha=0.5
x=rnorm(N,0,1)
x=ifelse(x>0,1,0) #the binary exposure. If want to use a continuous exposure, remove this line
e1=rnorm(N,0,1)
M=alpha*x+e1   #the mediator
lambda=0.01
rho=1
beta=1.2
c=-1
rateC=0.001
v=runif(n=N)
Tlat =(- log(v) / (lambda * exp(c*x+M*beta)))^(1 / rho) #the event time
C=rexp(n=N, rate=rateC)  #the censoring time
time=pmin(Tlat, C)
status <- as.numeric(Tlat <= C)
example2 <-cbind(x, M, time, status) #the dataset


## -----------------------------------------------------------------------------
data("weight_behavior")
#n.iter and n.burnin are set to be very small, should be adjusted
#binary predictor
test.b.c<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,c(14,12,13)],
                     y=weight_behavior[,1],n.iter=500,n.burnin = 100)
summary(test.b.c)

## -----------------------------------------------------------------------------
test.b.c<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,c(5:11,12:14)],
                       y=weight_behavior[,1],cova=weight_behavior[,2],mcov=weight_behavior[,c(2,5)],
                       mclist = list(1,2),n.iter=500,n.burnin = 100)
summary(test.ca.c)

## -----------------------------------------------------------------------------
test.c.c.2<- bma.bx.cy(pred=weight_behavior[,2], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],fpy=list(1,c("x","x^2")),n.iter=5,n.burnin = 1)
summary(test.c.c.2,method=1)

## -----------------------------------------------------------------------------
test.m.c<- bma.bx.cy(pred=weight_behavior[,2:4], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],n.iter=10,n.burnin = 1)
summary(test.m.c,method=3)

## -----------------------------------------------------------------------------
test.m.b<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,12:14],
                     y=weight_behavior[,15],cova=weight_behavior[,5],n.iter=500,n.burnin = 100)
summary(test.m.b,method=2)


## -----------------------------------------------------------------------------
test.m.t.1<- bma.bx.cy(pred=example2[,"x"], m=example2[,"M"],  y=Surv(example2[,"time"],example2[,"status"]), inits=function(){  list(r=1,lambda=0.01)},n.iter=10,n.burnin = 1)
temp1=summary(test.m.t.1)
print(temp1,method=1,RE=FALSE)

## -----------------------------------------------------------------------------
test.m.c<- bma.bx.cy(pred=weight_behavior[,2:4], m=weight_behavior[,12:13],
                     y=as.factor(weight_behavior[,14]),cova=weight_behavior[,5],n.iter=5,n.burnin = 1)
summary(test.m.c,method=3)

