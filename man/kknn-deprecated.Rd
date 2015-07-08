\name{kknn-deprecated}
%\alias{kknn-deprecated}
\alias{simulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deprecated Functions in Package kknn
}
\description{
These functions are provided for compatibility with older versions of R only, and may be defunct as soon as of the next release. 
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
\seealso{  \code{\link[base]{Defunct}} and \code{\link[base]{Deprecated}} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ classif }

