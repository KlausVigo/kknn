\name{specClust}
\alias{specClust}
\alias{plot.specClust}
\title{
Spectral Clustering
}
\description{
Spectral clustering based on k-nearest neighbor graph. 
}
\usage{
specClust(data, centers=NULL, nn = 7, method = "symmetric", gmax=NULL, ...)
\method{plot}{specClust}(x, ...)
}
\arguments{
\item{data}{Matrix or data frame.}
\item{centers}{number of clusters to estimate, if NULL the number is chosen automatical.}
\item{nn}{Number of neighbors considered.}
\item{method}{Normalisation of the Laplacian ("none", "symmetric" or "random-walk").}
\item{gmax}{maximal number of connected components.}
\item{x}{an object of class \code{specClust}}
\item{\dots}{Further arguments passed to or from other methods.}
}
\details{
\code{specClust} alllows to estimate several popular spectral clustering algorithms, for an overview see von Luxburg (2007).

The Laplacian is constructed from a from nearest neighbors and there are several kernels available. 
The eigenvalues and eigenvectors are computed using the binding in igraph to arpack.  
This should ensure that this algorithm is also feasable for larger datasets as the the the distances used have dimension n*m, where n is the number of observations and m the number of nearest neighbors. The Laplacian is sparse and has roughly n*m elements and only k eigenvectors are computed, where k is the number of centers.   
}
\value{
\code{specClust} returns a kmeans object or in case of k being a vector a list of kmeans objects.
}
\references{
U. von Luxburg (2007) A tutorial on spectral clustering, \emph{Stat Comput}, \bold{17}, 395--416

Ng, A., Jordan, M., Weiss, Y. (2002) On spectral clustering: analysis and an algorithm. In: Dietterich, T., Becker, S., Ghahramani, Z. (eds.)
 \emph{Advances in Neural Information Processing Systems}, \bold{14}, 849--856. MIT Press, Cambridge 


Lihi Zelnik-Manor and P. Perona (2004) Self-Tuning Spectral Clustering, \emph{Eighteenth Annual Conference on Neural Information Processing Systems, (NIPS)}


Shi, J. and Malik, J. (2000). Normalized cuts and image segmentation. \emph{IEEE Transactions on Pattern
Analysis and Machine Intelligence}, \bold{22 (8)}, 888--905
}
\author{ Klaus P. Schliep \email{klaus.schliep@gmail.com}}
\seealso{
\code{\link{kknn}}, \code{\link[igraph]{arpack}}, \code{\link{kmeans}}
}
\examples{
data(iris)
cl <- specClust(iris[,1:4], 3, nn=5)
pcol <- as.character(as.numeric(iris$Species))
pairs(iris[1:4], pch = pcol, col = c("green", "red", "blue")[cl$cluster])
table(iris[,5], cl$cluster)
}
\keyword{ cluster }

