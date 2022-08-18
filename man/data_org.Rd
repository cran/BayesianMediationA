\name{data_org}
%- Also NEED an '\ali
\alias{data_org}
\title{
Data Organization
}
\description{
The function is used to identify exposure, mediator, covariate, and outcome variables and organize the data into formats usable for the mediaiton analysis. The function is called by bma.bx.cy function before doing the Bayesian mediation analysis.
}
\usage{
data_org(pred, m, y, refy = rep(NA, ncol(data.frame(y))),
predref = rep(NA, ncol(data.frame(pred))),fpy = NULL,
deltap = rep(0.001, ncol(data.frame(pred))),
fmy = NULL, deltam = rep(0.001,ncol(data.frame(m))),
fpm = NULL, mref = rep(NA, ncol(data.frame(m))), cova = NULL,
mcov = NULL, mclist = NULL)
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
}
\value{
The function returns a list with transformed and organized data with the following items:
\item{N}{total number of obserations.}
\item{y_type }{the data type of y: 1 for continuous, 2 is binary, 3 for categorical, and 4 is time-to-event outcome.}
\item{y}{the response variable.}
\item{pred1}{exposure variable(s) in original formats, pred.}
\item{pred1.0}{exposure variable(s) in original formats if continuous, binarized if binary or categorical.}
\item{pred2}{all the exposure variables with transformations for y.}
\item{pred3}{all the exposure variables with transformations for y and continuous variable be pred+deltap.}
\item{cova}{the covariance data frame for explaining y.}
\item{pred.cont.der}{the derivative functions of the continuous exposure variables to explain y.}
\item{binpred2}{the column numbers of binary exposures in pred2.}
\item{catpred2}{the column numbers of categorical exposures in pred2. Each row is for one categorical exposure variable: column 1 is the starting column and column 2 is the ending column in pred2.}
\item{contpred2}{the column numbers of continuous exposures in pred2. Each row is for one continuous exposure variable: column 1 is the starting column and column 2 is the ending column in pred2.}
\item{binpred1}{the column numbers of binary exposures in pred1.}
\item{catpred1}{the column numbers of categorical exposures in pred1.}
\item{contpred1}{the column numbers of continuous exposures in pred1.}
\item{binpred1.0}{the column numbers of binary exposures in pred1.0.}
\item{catpred1.0}{the column numbers of categorical exposures in pred1.0. Each row is for one categorical exposure variable: column 1 is the starting column and column 2 is the ending column in pred2.}
\item{contpred1.0}{the column numbers of continuous exposures in pred1.0.}
\item{contpred3}{the column numbers of continuous exposures in pred3 and in pred.cont.der. Each row is for one continuous exposure variable: column 1 is the starting column and column 2 is the ending column in pred3.}
\item{npred}{the number of exposure variables.}
\item{m1}{the mediators in their original format.}
\item{m2}{the mediators all transformed to formats in explaining y.}
\item{m3}{transformed continuous mediators at the value of mediator+deltam.}
\item{m.cont.der}{derivative of the transformation functions for continuous mediators.}
\item{binm2}{the column number of binary mediators in m2.}
\item{catm2}{the column number of categorical mediators in m2. Each row is for one categorical mediator: column 1 is the starting column and column 2 is the ending column in m2.}
\item{contm2}{the column number of continuous mediators in m2. Each row is for one continuous mediator: column 1 is the starting column and column 2 is the ending column in m2.}
\item{binm1}{the column number of binary mediators in m1.}
\item{catm1}{the column number of categorical mediators in m1.}
\item{contm1}{the column number of continuous mediators in m1.}
\item{contm3}{the column numbers of continuous mediators in m3 and in m.cont.der. Each row is for one continuous mediator: column 1 is the starting column and column 2 is the ending column in m3 and m.cont.der.}
\item{nm}{total number of mediators.}
\item{pm}{the matrix of exposures with transformations that are used to explain mediators. The first column is all 0s.}
\item{pm.der}{the derivative matrix of exposures and transformed exposures that are used to explain mediators.}
\item{pm.idx}{a list, with the ith item gives the column numbers in pm that are used to explain the ith mediator.}
\item{pm.ind}{a matrix, with the ith row gives the column numbers in pm that are used to explain the ith mediator. }
\item{fpm.2}{a list similar to fpm. }
\item{binp}{the column number of binary exposures in pm.}
\item{catp}{the column number of categorical exposures in pm. Each row is for one categorical exposure: column 1 is the starting column and column 2 is the ending column in pm.}
\item{contp}{the column number of continuous exposures in pm. Each row is for one continuous exposure: column 1 is the starting column and column 2 is the ending column in pm.}
\item{p1}{the number of continuous mediators.}
\item{p2}{the number of binary mediators.}
\item{p3}{the number of categorical mediators.}
\item{mcov}{the matrix of all covariates for explaining mediators.}
\item{mind}{a matrix with the ith row indicate the columns of covariates in mcov that should be used to explain the ith mediator.}
}
\author{
Qingzhao Yu and Bin Li
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
