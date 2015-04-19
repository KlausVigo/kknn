\name{kknn}
\alias{kknn}
\alias{print.kknn}
\alias{summary.kknn}
\alias{predict.kknn}
\alias{kknn.dist}
\title{Weighted k-Nearest Neighbor Classifier }
\description{
Performs k-nearest neighbor classification of a test set using a
training set. For each row of the test set, the k nearest training
set vectors (according to Minkowski distance) are found, and the 
classification is done via the maximum of summed kernel densities. 
In addition even ordinal and continuous variables can be predicted.
}
\usage{
kknn(formula = formula(train), train, test, na.action = na.omit(), 
	k = 7, distance = 2, kernel = "optimal", ykernel = NULL, scale=TRUE,
	contrasts = c('unordered' = "contr.dummy", ordered = "contr.ordinal"))
kknn.dist(learn, valid, k = 10, distance = 2)     
}
\arguments{
  \item{formula}{A formula object.}
  \item{train}{Matrix or data frame of training set cases.}
  \item{test}{Matrix or data frame of test set cases.}
  \item{learn}{Matrix or data frame of training set cases.}
  \item{valid}{Matrix or data frame of test set cases.}  
  \item{na.action}{A function which indicates what should happen when the data contain 'NA's.}
  \item{k}{Number of neighbors considered.}
  \item{distance}{Parameter of Minkowski distance.}
  \item{kernel}{Kernel to use. Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)), 
	"biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv", "gaussian", "rank" and "optimal".} 
  \item{ykernel}{Window width of an y-kernel, especially for prediction of ordinal classes.}
  \item{scale}{logical, scale variable to have equal sd.}   	
  \item{contrasts}{A vector containing the 'unordered' and 'ordered' contrasts to use.}	
}
\details{
This nearest neighbor method expands knn in several directions. First it can be used not only for classification, but also for regression and ordinal classification.
Second it uses kernel functions to weight the neighbors according to their distances.
In fact, not only kernel functions but every monotonic decreasing function 
  	\eqn{f(x) \forall x>0}{f(x) for all x>0} will work fine. 

The number of neighbours used for the "optimal" kernel should be \eqn{ [ (2(d+4)/(d+2))^(d/(d+4)) k ]}, where k is the number that would be used for unweighted knn classification, i.e. kernel="rectangular". This factor \eqn{(2(d+4)/(d+2))^(d/(d+4))} is between 1.2 and 2 (see Samworth (2012) for more details).
}
\value{
  \code{kknn} returns a list-object of class \code{kknn} including the components 
  \item{fitted.values}{Vector of predictions.}
  \item{CL}{Matrix of classes of the k nearest neighbors.}
  \item{W}{Matrix of weights of the k nearest neighbors.}
  \item{D}{Matrix of distances of the k nearest neighbors.}
  \item{C}{Matrix of indices of the k nearest neighbors.}
  \item{prob}{Matrix of predicted class probabilities.}
  \item{response}{Type of response variable, one of \emph{continuous}, \emph{nominal} or \emph{ordinal}.}
  \item{distance}{Parameter of Minkowski distance.}
  \item{call}{The matched call.}
  \item{terms}{The 'terms' object used.}
}
\references{
Hechenbichler K. and Schliep K.P. (2004)  \emph{Weighted k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper 399, SFB 386, Ludwig-Maximilians University Munich
(\url{http://www.stat.uni-muenchen.de/sfb386/papers/dsp/paper399.ps})

Hechenbichler K. (2005)  \emph{Ensemble-Techniken und ordinale Klassifikation}, PhD-thesis

Samworth, R.J. (2012) \emph{Optimal weighted nearest neighbour classifiers.} Annals of Statistics, 40, 2733-2763.
(avaialble from \url{http://www.statslab.cam.ac.uk/~rjs57/Research.html}) 
}
\author{ Klaus P. Schliep \email{klaus.schliep@gmail.com} \cr Klaus Hechenbichler}
 


\seealso{ \code{\link[kknn]{train.kknn}}, \code{\link[kknn]{simulation}}, \code{\link[class]{knn}} and \code{\link[class]{knn1}} }
\examples{
library(kknn)

data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit)
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
	[(iris.valid$Species != fit)+1])

data(ionosphere)
ionosphere.learn <- ionosphere[1:200,]
ionosphere.valid <- ionosphere[-c(1:200),]
fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
table(ionosphere.valid$class, fit.kknn$fit)
(fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
(fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)
}
\keyword{ classif }

