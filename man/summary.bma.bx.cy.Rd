\name{summary.bma.bx.cy}
\alias{summary.bma.bx.cy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary function for the bma.bx.cy object
}
\description{
Calculate the Bayesian estimates, standard deviations and credible sets for the mediation effects.
}
\usage{
\method{summary}{bma.bx.cy}(object, ..., plot = TRUE, RE = TRUE,
quant = c(0.025, 0.25, 0.5, 0.75, 0.975), digit = 4, method = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
The bma.bx.cy object which is the outcome of the function bam.bx.cy.
}
  \item{\dots}{
arguments to be passed to methods.
}
  \item{plot}{
If plot is True (default), plot the estimated (relative) effects with the predicted credit sets, using method specified by method.
}
  \item{RE}{
If RE is true, the relative effect plot is drawn.
}
  \item{quant}{
The quantiles of the estimates to be reported. By default, the quantiles are c(0.025, 0.25, 0.5, 0.75, 0.975).
}
  \item{digit}{
The number of digit to print in the result.
}
  \item{method}{
The method is printing to be used for estimation.
}
}
\value{
This function returns an object that has the following items:
  \item{result1 }{The result of (in)direct estimates from method 1.}
  \item{result1.re }{The result of relative (in)direct estimates from method 1.}
  \item{result2 }{The result of (in)direct estimates from method 2.}
  \item{result2.re }{The result of relative (in)direct estimates from method 2.}
  \item{result3 }{The result of (in)direct estimates from method 3.}
  \item{result3.re }{The result of relative (in)direct estimates from method 3.}
  \item{result4 }{The result of (in)direct estimates from method 4.}
  \item{result4.re }{The result of relative (in)direct estimates from method 4.}
}
\author{
Qingzhao Yu and Ari Li.
}
\examples{
# See examples at bma.bx.cy.
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
