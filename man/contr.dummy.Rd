\name{contr.dummy}
\alias{contr.dummy}
\alias{contr.metric}
\alias{contr.ordinal}
\title{Contrast Matrices}
\description{
Returns a matrix of contrasts.
}
\usage{
contr.dummy(n, contrasts = TRUE)
contr.ordinal(n, contrasts = TRUE)
contr.metric(n, contrasts = TRUE)
}
\arguments{
\item{n}{A vector containing levels of a factor, or the number of levels.}
\item{contrasts}{A logical value indicating whether contrasts should be computed.}
}
\details{
\code{contr.dummy} is standard dummy-coding, \code{contr.metric} has the same 
effect like \code{as.numeric} (makes sense of course only for ordered variables). 
\code{contr.ordinal} computes contrasts for ordinal variables.
}	
\value{
A matrix with \emph{n} rows and \emph{n-1} columns for \code{contr.ordinal}, 
a matrix with \emph{n} rows and \emph{n} columns for \code{contr.dummy} 
and a vector of length \emph{n} for \code{contr.metric}.
}
\references{Hechenbichler K. and Schliep K.P. (2004)  \emph{Weighted k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper 399, SFB 386, Ludwig-Maximilians University Munich
(\url{http://www.stat.uni-muenchen.de/sfb386/papers/dsp/paper399.ps})}
\author{Klaus P. Schliep \email{klaus.schliep@gmail.com} }
 
\seealso{\code{\link[stats]{contrasts}}, \code{\link[stats:contrast]{contr.poly}} and \code{\link[MASS]{contr.sdif}} }
\examples{
contr.metric(5)
contr.ordinal(5)
contr.dummy(5)
}
\keyword{classif}
\keyword{design}
