\name{simulation}
\alias{simulation}
\title{ Crossvalidation procedure to test prediction accuracy }
\description{
\code{simulation} tests prediction accuracy of regression and/or 
classification techniques via simulation of different test sets.
}
\usage{
simulation(formula, data, runs = 10, train = TRUE, k = 11, ...)
}
\arguments{
  \item{formula}{A formula object.}
  \item{data}{Matrix or data frame. }
  \item{runs}{Number of crossvalidation runs. }
  \item{train}{A logical value. If TRUE the training procedure for selecting optimal values of k and kernel is performed.}
  \item{k}{Number or maximal number of neighbors considered, dependent of choice for train.}	
  \item{\dots}{Further arguments passed to or from other methods.}
}
\value{
  A matrix, containing the mean and variance of the misclassification
error, the absolute and the squared distances.
}
\references{
Hechenbichler K. and Schliep K.P. (2004)  \emph{Weighted k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper 399, SFB 386, Ludwig-Maximilians University Munich
(\url{http://www.stat.uni-muenchen.de/sfb386/papers/dsp/paper399.ps}) 
}
\author{ Klaus P. Schliep \email{klaus.schliep@gmail.com} }
\seealso{  \code{\link[kknn]{kknn}} and \code{\link[kknn]{train.kknn}} }
\examples{
library(kknn)
data(miete)
simulation(nmqm ~ wfl + bjkat + zh, data = miete, runs = 5, kernel = "triangular", k = 15)
simulation(wflkat ~ nm + bjkat + zh, data = miete, runs = 5)
simulation(zh ~ wfl + bjkat + nmqm, data = miete, runs = 5)

}
\keyword{ classif }% at least one, from doc/KEYWORDS
