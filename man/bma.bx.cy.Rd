\name{bma.bx.cy}
\alias{bma.bx.cy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian Mediation Analysis
}
\description{
The function is to perform the Bayesian mediation anlaysis.
}
\usage{
bma.bx.cy(pred, m, y, refy = rep(NA, ncol(data.frame(y))),
predref = rep(NA, ncol(data.frame(pred))),
fpy = NULL, deltap = rep(0.001, ncol(data.frame(pred))), fmy = NULL,
deltam = rep(0.001,ncol(data.frame(m))),
fpm = NULL, mref = rep(NA, ncol(data.frame(m))), cova = NULL, mcov = NULL,
mclist = NULL, inits = NULL, n.chains = 1, n.iter = 1100, n.burnin = 100,
n.thin = 1, mu = NULL,Omega = NULL, Omegac = NULL, muc = NULL,
mucv = NULL, Omegacv = NULL, mu0.1 = NULL, Omega0.1 = NULL,
mu1.1 = NULL, Omega1.1 = NULL, mu0.a = NULL, Omega0.a = NULL,
mu1.a = NULL, Omega1.a = NULL, mu0.b =NULL,
Omega0.b = NULL, mu1.b = NULL, Omega1.b = NULL, mu0.c = NULL,
Omega0.c = NULL, mu1.c = NULL, Omega1.c = NULL, preci = 1e-06,
tmax = Inf, multi = NULL,filename=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred}{
a vector or matrix of exposure variable(s).
}
  \item{m}{
a data frame with all mediators.
}
  \item{y}{
the response variable.
}
  \item{refy}{
the reference group of the response variable if y is binary or categorical.
}
  \item{predref}{
the reference group(s) of the exposure variable(s) by the column of pred.
}
  \item{fpy}{
the transformation function expressions on pred in explaining y (eg, list(1,c("x^2","log(x)"))). The first item lists column numbers/variable names of the exposure variable in pred, which needs to be transformed. By that order, each of the rest items of fpy list the transformation functional expressions for each exposure. The exposures not specified in the list will not be transformed in any way in explaining y. For example, list(1,c("x^2","log(x)")) means that the first column of the pred will be transformed to square and log forms in the function to explain y.
}
  \item{deltap}{
the vector of differences in the exposure variable when calculating the partial differences in methods 2 and 3. The vector is in the order of columns of pred. By default, 0.001 for continuous variables and 1 for binary or categorical exposures.
}
  \item{fmy}{
the transformation function expressions on m in explaining y. The first item lists column numbers/variable names of the mediator in m, which needs to be transformed. By that order, each of the rest items of fmy list the transformation functional expressions for each mediator. The mediators not specified in the list will keep the original formats in explaining y.}
  \item{deltam}{
the vector of differences in the mediators in calculating the partial difference in y for method 2. The vector is in the order of columns of m. By default, 0.001 for continuous variables and 1 for binary or categorical mediators.
}
  \item{fpm}{
the transformation-function-expression list on exposure variable(s) (pred) in explaining mediators (m). The first item is a matrix with two columns: the first column is the column numbers of the mediators in m, which should be explained by the transformed predictor(s). The second column indicates the column number of the exposure in pred that will be transformed to explain the mediator identified by the 1st column of the same row. By the order of the rows of the first item, each of the rest items of fpm lists the transformation functional expressions for the exposure (identified by column 2) in explaining each mediator (identified by column 1). The mediators not specified in the list will be explained by the original format of the exposures in pred. For example, fpm=list(matrix(c(1,2,1,1),2,2), "x^2",c("x","x^2")) means that pred[,1]^2 is used to explain m[,1], and both pred and pred[,1]^2 are used to explain m[,2].
}
  \item{mref}{
the reference group of the mediators in the order of the columns of m.
}
  \item{cova}{
the covariates that are used to explain y in addition to pred, m and/or their transformations.
}
  \item{mcov}{
the covariates that are used to explain mediators in additon to pred and/or their tranformations.
}
  \item{mclist}{
If mclist is NULL (by default), all covariates in mcov are used for all mediators in m. Otherwise, the first item of mclist lists all column numbers/names of mediators in m that are to be explained by covariates in mcov, the following items give the covariates in mcov for the mediators in the order of the first item. Use NA if no mcov is to be used.
}
  \item{inits}{
the initial values for parameters in models. For example, inits<-function(){list(alpha=0,beta=0,c=0,var1=0.5,r=1,lamda=0.01)}.
}
  \item{n.chains}{
number of Markov chains. By fault, the number is 1.
}
  \item{n.iter}{
number of total iterations per chain. The default number is 1100.
}
  \item{n.burnin}{
length of burn in, i.e. number of iterations to discard at the beginning. Default is 100.
}
  \item{n.thin}{
thinning rate. Must be a positive integer. Set n.thin > 1 to save memory and computation time if n.iter is large. Default is max(1, floor(n.chains * (n.iter-n.burnin) / 1000)) which will only thin if there are at least 2000 simulations.
}
  \item{mu}{
the prior mean vector (of length P, where P is number of mediators) for coefficients of mediators in the final model for y. By default, all prior mean is 0.
}
  \item{Omega}{
the prior variance-covariance matrix for the coefficients of mediators in the final model for y. The dimension is P*P. By default, all variance-covariance matrix is diagnal with precision term defined by preci.
}
  \item{Omegac}{
the prior variance-covariance matrix for the coefficients of exposure(s) in the final model for y. The dimention is c2*c2. c2 is number of expsoure(s) to predict y.
}
  \item{muc}{
the prior mean vector (of length p2) for coefficients of exposure(s) in the final model for y.
}
  \item{mucv}{
the prior mean vector for coefficients of covariate(s) in the final model for y.
}
  \item{Omegacv}{
the prior variance-covariance matrix for the coefficients of covariate(s) in the final model for y.
}
  \item{mu0.1}{
the prior mean matrix for coefficients of intercept and covariate(s) in the model for predicting mediators. The dimension is P*nmc, where P is the total number of mediators, and nmc is the total number of covariates (1 is a covarite when intercept is fitted). Note that if there are different numbers of covariates for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega0.1}{
the prior variance-covariance matrix for coefficients of intercept and covariate(s) in the model for predicting mediators. The dimension is nmc*nmc. Note that if there are different numbers of covariates for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{mu1.1}{
the prior mean matrix for coefficients of exposure(s) in the model for predicting mediators. The dimension is P*c1, where P is the total number of mediators, and c1 is the total number of original exposure(s). Note that if there are different numbers or transformation of exposure(s) for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega1.1}{
the prior variance-covariance matrix for coefficients of exposure(s) in the model for predicting mediators. The dimension is c1*c1. Note that if there are different numbers of exposure(s) for coefficients, the priors need to be specified in the bug model defined in filename.
}
  \item{mu0.a}{
the prior mean matrix for coefficients of intercept and covariate(s) in the model for predicting continuous mediators. The dimension is p1*nmc, where p1 is the total number of continuous mediators, and nmc is the total number of covariates (1 is a covarite when intercept is fitted). Note that if there are different numbers of covariates for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega0.a}{
the prior variance-covariance matrix for coefficients of intercept and covariate(s) in the model for predicting continuous mediators. The dimension is nmc*nmc. Note that if there are different number of covariate(s) for different mediators, the priors need to be specified in the bug model defined in filename.
}
  \item{mu1.a}{
the prior mean matrix for coefficients of exposure(s) in the model for predicting continuous mediators. The dimension is p1*c1, where p1 is the total number of continuous mediators, and c1 is the total number of original exposure(s). Note that if there are different numbers or transformation of exposure(s) for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega1.a}{
the prior variance-covariance matrix for coefficients of exposure(s) in the model for predicting continuous mediators. The dimension is c1*c1. Note that if there are different numbers of exposure(s) for each mediator, the priors need to be specified in the bug model defined in filename.
}
  \item{mu0.b}{
the prior mean matrix for coefficients of intercept and covariate(s) in the model for predicting binary mediators. The dimension is p2*nmc, where p2 is the total number of binary mediators, and nmc is the total number of covariates (1 is a covarite when intercept is fitted). Note that if there are different numbers of covariates for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega0.b}{
the prior variance-covariance matrix for coefficients of intercept and covariate(s) in the model for predicting binary mediators. The dimension is nmc*nmc. Note that if there are different number of covariate(s) for different mediators, the priors need to be specified in the bug model defined in filename.
}
  \item{mu1.b}{
the prior mean matrix for coefficients of exposure(s) in the model for predicting binary mediators. The dimension is p2*c1, where p2 is the total number of binary mediators, and c1 is the total number of original exposure(s). Note that if there are different numbers or transformation of exposure(s) for different mediators, the priors need to specified in the bug model defined in filename.}
  \item{Omega1.b}{
the prior variance-covariance matrix for coefficients of exposure(s) in the model for predicting binary mediators. The dimension is c1*c1. Note that if there are different numbers of exposure(s) for each mediator, the priors need to be specified in the bug model defined in filename.
}
  \item{mu0.c}{
the prior mean matrix for coefficients of intercept and covariate(s) in the model for predicting categorical mediators. The dimension is p3*cat1*nmc, where p3 is the total number of categorical mediators, cat1 is the biggest number of categories for all categorical mediators, and nmc is the total number of covariates (1 is a covarite when intercept is fitted). Note that if there are different numbers of covariates for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega0.c}{
the prior variance-covariance matrix for coefficients of intercept and covariate(s) in the model for predicting categorical mediators. The dimension is nmc*nmc. Note that if there are different number of covariate(s) for different mediators, the priors need to be specified in the bug model defined in filename.
}
  \item{mu1.c}{
the prior mean matrix for coefficients of exposure(s) in the model for predicting categorical mediators. The dimension is p3*c1, where p3 is the total number of categorical mediators, and c1 is the total number of original exposure(s). Note that if there are different numbers or transformation of exposure(s) for different mediators, the priors need to specified in the bug model defined in filename.
}
  \item{Omega1.c}{
the prior variance-covariance matrix for coefficients of exposure(s) in the model for predicting categorical mediators. The dimension is c1*c1. Note that if there are different numbers of exposure(s) for each mediator, the priors need to be specified in the bug model defined in filename.
}
  \item{preci}{
define the precision term in the diagnoal variance-covariance matrix.
}
  \item{tmax}{
the maximum suvival time if the outcome is time-to-event.
}
  \item{multi}{
if true, report the productive effect.
}
  \item{filename}{
If specified, filename gives the bugs model with folder direction, e.g., "C:/bugs.txt". If not specified, a bugs model will be generated automatically.
}
}
\details{
In this function, data_org is called first and then jags is called for a Bayesian model fitting. Finally, three different Bayesian mediation analysis methods are used to make inferences on mediation effects.
}
\value{
  \item{aie1 }{The average indiect effect from method 1. The dimension is (n.iter-n.burnin)*(# of mediators)*(# of exposures).}
  \item{ade1 }{The average diect effect from method 1. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{ate1 }{The average total effect from method 1. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{aie2 }{The average indiect effect from method 2. The dimension is (n.iter-n.burnin)*(# of mediators)*(# of exposures).}
  \item{ade2 }{The average diect effect from method 2. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{ate2 }{The average total effect from method 2. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{aie3 }{The average indiect effect from method 3. The dimension is (n.iter-n.burnin)*(# of mediators)*(# of exposures).}
  \item{ade3 }{The average diect effect from method 3. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{ate3 }{The average total effect from method 3. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{aie4 }{The average indiect effect from method 4. The dimension is (n.iter-n.burnin)*(# of mediators)*(# of exposures).}
  \item{ade4 }{The average diect effect from method 4. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{ate4 }{The average total effect from method 4. The dimension is (n.iter-n.burnin)*(# of exposures).}
  \item{sims.list }{The Bayesian model fitting results from japs.}
  \item{data0 }{The fitting results from data_org.}
  \item{omu3 }{When the outcome is time-to-event, omu3 gives the mean survival at the original data for method 3.}
  \item{omu4 }{When the outcome is time-to-event, omu4 gives the mean survival at the original data for method 4.}
  }
\author{
Qingzhao Yu and Bin Li
}
\examples{
data("weight_behavior")
#n.iter and n.burnin are set to be very small, should be adjusted
#binary predictor
test.b.c<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,c(14,12,13)],
                     y=weight_behavior[,1],n.iter=5,n.burnin = 1)
summary(test.b.c)
\donttest{
#categorical predictor
test.ca.c<- bma.bx.cy(pred=weight_behavior[,4], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],n.iter=5,n.burnin = 1)
summary(test.ca.c)

#use covariate for y
test.b.c.2<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],cova=weight_behavior[,2],n.iter=10,n.burnin = 1)
summary(test.b.c.2)

#use covariate for mediators
test.b.c.3<- bma.bx.cy(pred=weight_behavior[,3], m=weight_behavior[,c(9,12:14)],
                       y=weight_behavior[,1],mcov=weight_behavior[,c(2,5)],
                       mclist = list(1,2),n.iter=5,n.burnin = 1)
summary(test.b.c.3)

#use continuous predictor
test.c.c<- bma.bx.cy(pred=weight_behavior[,2], m=weight_behavior[,12:14],
                       y=weight_behavior[,1],n.iter=5,n.burnin = 1)
summary(test.c.c,method=3)

#use transfered continuous predictor for y
test.c.c.2<- bma.bx.cy(pred=weight_behavior[,2], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],fpy=list(1,c("x","x^2")),n.iter=5,n.burnin = 1)
summary(test.c.c.2,method=1)

#multiple predictors
test.m.c<- bma.bx.cy(pred=weight_behavior[,2:4], m=weight_behavior[,12:14],
                     y=weight_behavior[,1],n.iter=10,n.burnin = 1)
summary(test.m.c,method=3)

##binary outcome
test.m.b<- bma.bx.cy(pred=weight_behavior[,2:4], m=weight_behavior[,12:14],
                     y=weight_behavior[,15],cova=weight_behavior[,5],n.iter=5,n.burnin = 1)
summary(test.m.b,method=2)

##time-to-event outcome
#use a simulation
set.seed(1)
N=100

alpha=0.5
x=rnorm(N,0,1)
x=ifelse(x>0,1,0)
e1=rnorm(N,0,1)
M=alpha*x+e1
lambda=0.01
rho=1
beta=1.2
c=-1
rateC=0.001
v=runif(n=N)
Tlat =(- log(v) / (lambda * exp(c*x+M*beta)))^(1 / rho)
C=rexp(n=N, rate=rateC)
time=pmin(Tlat, C)
status <- as.numeric(Tlat <= C)

test.m.t.1<- bma.bx.cy(pred=x, m=M,y=Surv(time,status),inits=function(){
  list(r=1,lambda=0.01)},n.iter=10,n.burnin = 1)
temp1=summary(test.m.t.1)
print(temp1,method=1,RE=FALSE)

##categorical outcome
test.m.c<- bma.bx.cy(pred=weight_behavior[,2:4], m=weight_behavior[,12:13],
                     y=weight_behavior[,14],cova=weight_behavior[,5],n.iter=5,n.burnin = 1)
summary(test.m.c,method=3)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
