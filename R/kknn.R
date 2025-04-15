#' Contrast Matrices
#' 
#' Returns a matrix of contrasts.
#' 
#' \code{contr.dummy} is standard dummy-coding, \code{contr.metric} has the
#' same effect like \code{as.numeric} (makes sense of course only for ordered
#' variables).  \code{contr.ordinal} computes contrasts for ordinal variables.
#' 
#' @aliases contr.dummy contr.metric contr.ordinal
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be
#' computed.
#' @return A matrix with \emph{n} rows and \emph{n-1} columns for
#' \code{contr.ordinal}, a matrix with \emph{n} rows and \emph{n} columns for
#' \code{contr.dummy} and a vector of length \emph{n} for \code{contr.metric}.
#' @author Klaus P. Schliep \email{klaus.schliep@@gmail.com}
#' @seealso \code{\link[stats]{contrasts}},
#' \code{\link[stats:contrast]{contr.poly}} and \code{\link[MASS]{contr.sdif}}
#' @references Hechenbichler K. and Schliep K.P. (2004) \emph{Weighted
#' k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper
#' 399, SFB 386, Ludwig-Maximilians University Munich
#' (\doi{10.5282/ubm/epub.1769})
#' @keywords classif design
#' @examples
#' 
#' contr.metric(5)
#' contr.ordinal(5)
#' contr.dummy(5)
#' 
#' @export contr.dummy
contr.dummy <- function (n, contrasts = TRUE) 
{
    if (length(n) <= 1) {
        if (is.numeric(n) && length(n) == 1 && n > 1) 
            levels <- 1:n
        else stop("contrasts are not defined for 0 degrees of freedom")
    }
    else levels <- n
    	lenglev <- length(levels)
        cont <- array(0, c(lenglev, lenglev), list(levels, levels))
        cont[col(cont) == row(cont)] <- 1
    cont
}


#' @export
contr.ordinal <- function (n, contrasts = TRUE) 
{
    if (length(n) <= 1) {
        if (is.numeric(n) && length(n) == 1 && n > 1) 
            levels <- 1:n
        else stop("contrasts are not defined for 0 degrees of freedom")
    }
    else levels <- n
    	lenglev <- length(levels)
        cont <- array(0.5, c(lenglev, lenglev - 1), list(levels, NULL))
        cont[lower.tri(cont)] <- -0.5
    cont
}


#' @export
contr.metric <- function(n, contrasts = TRUE) 
{	
    if (length(n) <= 1) {
        if (is.numeric(n) && length(n) == 1 && n > 1) 
            levels <- 1:n
        else stop("contrasts are not defined for 0 degrees of freedom")
    }
    else levels <- n
    lenglev <- length(levels)
    cont <- array((1:lenglev)-(1+lenglev)/2 , c(lenglev,1), list(levels,NULL)) 
    cont	
}


contr.int <- function (n, contrasts = TRUE) 
{
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1) 
      levels <- as.integer(1:n)
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(as.integer(1:lenglev), c(lenglev, 1), 
                list(levels, NULL))
  cont
}



get_contrasts <- function(mt, contrasts){
  dataClasses <- attr(mt, "dataClasses")
  unorderedVariables <- names(dataClasses[dataClasses %in% c("factor", "character")])
  orderedVariables   <- names(dataClasses[dataClasses %in% c("ordered")])
  if (length(unorderedVariables) == 0 && length(orderedVariables) == 0) {
    contrasts.arg <- NULL
  } else {
    contrasts.arg <- list()
    contrasts.arg[unorderedVariables] <- list(contrasts[["unordered"]])
    contrasts.arg[orderedVariables]   <- list(contrasts[["ordered"]])
  }
  contrasts.arg
}


optKernel <- function(k, d=1){
   1/k*(1 + d/2 - d/(2*k^(2/d)) * ( (1:k)^(1+2/d) - (0:(k-1))^(1+2/d)  ))
}


#' Weighted k-Nearest Neighbor Classifier
#' 
#' Performs k-nearest neighbor classification of a test set using a training
#' set. For each row of the test set, the k nearest training set vectors
#' (according to Minkowski distance) are found, and the classification is done
#' via the maximum of summed kernel densities.  In addition even ordinal and
#' continuous variables can be predicted.
#' 
#' This nearest neighbor method expands knn in several directions. First it can
#' be used not only for classification, but also for regression and ordinal
#' classification. Second it uses kernel functions to weight the neighbors
#' according to their distances. In fact, not only kernel functions but every
#' monotonic decreasing function \eqn{f(x) \forall x>0}{f(x) for all x>0} will
#' work fine.
#' 
#' The number of neighbours used for the "optimal" kernel should be \eqn{ [
#' (2(d+4)/(d+2))^(d/(d+4)) k ]}, where k is the number that would be used for
#' unweighted knn classification, i.e. kernel="rectangular". This factor
#' \eqn{(2(d+4)/(d+2))^(d/(d+4))} is between 1.2 and 2 (see Samworth (2012) for
#' more details).
#' 
#' @aliases kknn print.kknn summary.kknn predict.kknn kknn.dist
#' @param formula A formula object.
#' @param train Matrix or data frame of training set cases.
#' @param test Matrix or data frame of test set cases.
#' @param learn Matrix or data frame of training set cases.
#' @param valid Matrix or data frame of test set cases.
#' @param na.action A function which indicates what should happen when the data
#' contain 'NA's.
#' @param k Number of neighbors considered.
#' @param distance Parameter of Minkowski distance.
#' @param kernel Kernel to use. Possible choices are "rectangular" (which is
#' standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)),
#' "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv",
#' "gaussian", "rank" and "optimal".
#' @param ykernel Window width of an y-kernel, especially for prediction of
#' ordinal classes.
#' @param scale logical, scale variable to have equal sd.
#' @param contrasts A vector containing the 'unordered' and 'ordered' contrasts
#' to use.
#' @return \code{kknn} returns a list-object of class \code{kknn} including the
#' components \item{fitted.values}{Vector of predictions.} \item{CL}{Matrix of
#' classes of the k nearest neighbors.} \item{W}{Matrix of weights of the k
#' nearest neighbors.} \item{D}{Matrix of distances of the k nearest
#' neighbors.} \item{C}{Matrix of indices of the k nearest neighbors.}
#' \item{prob}{Matrix of predicted class probabilities.} \item{response}{Type
#' of response variable, one of \emph{continuous}, \emph{nominal} or
#' \emph{ordinal}.} \item{distance}{Parameter of Minkowski distance.}
#' \item{call}{The matched call.} \item{terms}{The 'terms' object used.}
#' @importFrom stats as.formula dbeta delete.response dnorm na.omit model.frame 
#' model.response model.matrix qnorm sd
#' @author Klaus P. Schliep \email{klaus.schliep@@gmail.com} \cr Klaus
#' Hechenbichler
#' @seealso \code{\link[kknn]{train.kknn}}
#' @references Hechenbichler K. and Schliep K.P. (2004) \emph{Weighted
#' k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper
#' 399, SFB 386, Ludwig-Maximilians University Munich
#' (\doi{10.5282/ubm/epub.1769})
#' 
#' Hechenbichler K. (2005) \emph{Ensemble-Techniken und ordinale
#' Klassifikation}, PhD-thesis
#' 
#' Samworth, R.J. (2012) \emph{Optimal weighted nearest neighbour classifiers.}
#' Annals of Statistics, 40, 2733-2763. (available from
#' \url{http://www.statslab.cam.ac.uk/~rjs57/Research.html})
#' @keywords classif
#' @examples
#' 
#' library(kknn)
#' 
#' data(iris)
#' m <- dim(iris)[1]
#' val <- sample(1:m, size = round(m/3), replace = FALSE, 
#' 	prob = rep(1/m, m)) 
#' iris.learn <- iris[-val,]
#' iris.valid <- iris[val,]
#' iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
#' 	kernel = "triangular")
#' summary(iris.kknn)
#' fit <- fitted(iris.kknn)
#' table(iris.valid$Species, fit)
#' pcol <- as.character(as.numeric(iris.valid$Species))
#' pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
#' 	[(iris.valid$Species != fit)+1])
#' 
#' data(ionosphere)
#' ionosphere.learn <- ionosphere[1:200,]
#' ionosphere.valid <- ionosphere[-c(1:200),]
#' fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
#' table(ionosphere.valid$class, fit.kknn$fit)
#' (fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
#' 	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
#' table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
#' (fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15, 
#' 	kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
#' table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)
#' 
#' @rdname kknn
#' @export kknn
kknn <-  function (formula = formula(train), train, test, na.action=na.omit(), 
                   k = 7, distance = 2, kernel = "optimal", ykernel = NULL, 
                   scale=TRUE, contrasts=c('unordered'="contr.dummy", 
                                           ordered="contr.ordinal")) 
{
    if(is.null(ykernel)) ykernel <- 0

    weight.y <- function(l=1,diff = 0){
        k <- diff+1
        result <- matrix(0,l,l)
        diag(result) <- k
        for(i in 1:(k-1)){
            for(j in 1:(l-i)){
                result[j,j+i] <- k-i
                result[j+i,j] <- k-i
            }
        }
        result  
    }

    kernel <- match.arg(kernel, c("rectangular", "triangular", "epanechnikov",
                                  "biweight", "triweight", "cos", "inv", 
                                  "gaussian", "rank", "optimal"), FALSE)
    ca <- match.call()
    response <- NULL
#    old.contrasts <- getOption('contrasts')
#    options(contrasts=contrasts)

    formula <- as.formula(formula)

    mf <- model.frame(formula, data = train)    
    mt <- attr(mf, "terms")
#reformulate(, intercept = FALSE
    
    mt2 <- delete.response(mt)
    cl <- model.response(mf)
    d <- sum(attr(mt, "order"))

    if(is.ordered(cl)) {
        response<-"ordinal"
        lev <- levels(cl)
	}
    if(is.numeric(cl)) response<-"continuous"
    if(is.factor(cl) & !is.ordered(cl)){
        response<-"nominal"
	    lev <- levels(cl)
	}
	
    if(distance<=0)stop('distance must >0')
    if(k<=0)stop('k must >0')

    contrasts.arg <- get_contrasts(mt, contrasts)  
    
    learn <- model.matrix(mt, mf, contrasts.arg=contrasts.arg)
    valid <- model.matrix(mt2,test, contrasts.arg=contrasts.arg)

    m <- dim(learn)[1]
    p <- dim(valid)[1]
    q <- dim(learn)[2]

    if(k>m) stop('k must be smaller or equal the number of rows of the training
                 set')
    
    ind <- attributes(learn)$assign 

    d.sd <- numeric(length(ind))+1
    we <- numeric(length(ind))+1
    
    d.sd <- apply(learn, 2, stats::var)
    for (i in unique(ind)){
    	d.sd[ind==i] <- sqrt(mean(d.sd[ind==i]))
    	we[ind==i] <- 1/sum(ind==i)
    	}

    we[d.sd==0] <- 0
    d.sd[d.sd==0] <- 1

    if(scale){
# change 5.3.2013      
        learn <- sweep(learn, 2L, d.sd, "/", check.margin = FALSE) 
        valid <- sweep(valid, 2L, d.sd, "/", check.margin = FALSE) 
    } 
# ordering allows branch and bound in distance computation
    ord <- order(we * apply(learn, 2, sd), decreasing=TRUE)

    we <- we[ord]
    learn <- learn[,ord, drop=FALSE]
    valid <- valid[,ord, drop=FALSE]

    Euclid <- FALSE
    if(distance==2) Euclid <- TRUE
    if(Euclid) dmtmp <- .C(dmEuclid, as.double(learn), as.double(valid), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm=double((k+1L) * p), cl=integer((k+1L) * p), k=as.integer(k+1), 
        as.double(we))

    else dmtmp <- .C(dm, as.double(learn), as.double(valid), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm=double((k+1L) * p), cl=integer((k+1L) * p), k=as.integer(k+1), 
        as.double(distance),as.double(we))
    
    D <- matrix(dmtmp$dm, nrow = p, ncol = k + 1)
    C <- matrix(dmtmp$cl, nrow = p, ncol = k + 1)
    maxdist <- D[, k + 1]
    maxdist[maxdist < 1.0e-6] <- 1.0e-6
    D <- D[, 1:k, drop=FALSE]
    C <- C[, 1:k, drop=FALSE]+1
    CL <- matrix(cl[C], nrow = p, ncol = k)     
    
    if(response!="continuous"){
        l <- length(lev)
        weightClass <- matrix(0, p, l)
    }
    if(response=="continuous"){
        weightClass <- NULL
    }

    W <- D/maxdist
    W <- pmin(W,1-(1e-6))
    W <- pmax(W,1e-6)

#	
# Kernels
#	
    if (kernel=="rank") W <- (k+1)-t(apply(as.matrix(D),1,rank))
    if (kernel=="inv") W <- 1/W
    if (kernel=="rectangular") W <- matrix(1,nrow = p, ncol = k)
    if (kernel=="triangular") W <- 1-W	 	
    if (kernel=="epanechnikov") W <- 0.75*(1-W^2)
    if (kernel=="biweight") W <- dbeta((W+1)/2,3,3)	 	
    if (kernel=="triweight") W <- dbeta((W+1)/2,4,4)	 	
    if (kernel=="cos") W <- cos(W*pi/2)
    if (kernel=="triweights") W <- 1
    if (kernel=="gaussian"){
	    alpha <- 1/(2*(k+1))
	    qua <- abs(qnorm(alpha))
	    W <- W*qua
      W <- dnorm(W, sd = 1)
    }
    if (kernel == "optimal") {
        W <- rep(optKernel(k, d=d), each=p)
    } 
    W <- matrix(W, p, k)

if(response!="continuous"){
    for (i in 1:l) {
		weightClass[, i] <- rowSums(W * (CL == lev[i]))	
    }
    weightClass <- weightClass/rowSums(weightClass)	
	colnames(weightClass) <- lev
    }
    
if (response=="ordinal") {
   
    blub <- length(lev)
    weightClass <- weightClass%*%weight.y(blub,ykernel)
    weightClass <- weightClass/rowSums(weightClass)	
    weightClass <- t(apply(weightClass, 1, cumsum))
    colnames(weightClass) <- lev
       	
    fit <- numeric(p)
    for (i in 1:p) fit[i] <- min((1:l)[weightClass[i, ] >= 0.5])
    fit <- ordered(fit, levels = 1:l, labels = lev)
    }
	if(response=="nominal"){ 
		fit <- apply(weightClass, 1, order, decreasing = TRUE)[1,]
		fit <- factor(fit, levels = 1:l, labels = lev)

	if(kernel=="rectangular" && k>1){
		blub <- apply(weightClass, 1, rank, ties.method = "max")
		indices <- (1:p)[colSums(blub==l)>1]
		blub <- t(blub)
		nM <- matrix(0,p,l) 
		colnames(nM) <- lev
		for(i in 1:l) nM[,i] <- apply((CL==lev[i]) %*% diag(1:k) ,1,max)

		nM <- (blub==l)*nM  
		nM[nM==0] <- k+1
		fitv <- numeric(p)
		for(i in indices) fitv[i] <- which(nM[i,]==min(nM[i,]))
		fit[indices] <- factor(fitv[indices], levels = 1:l, labels = lev)
		}
	}
	if(response=="continuous") fit <- rowSums(W*CL)/pmax(rowSums(W), 1e-6) 
    #fit <- rowSums(W*CL)/sapply(rowSums(W),'max',1e-6) 
#    options('contrasts'=old.contrasts)	
	
    result <- list(fitted.values=fit, CL=CL, W=W, D=D, C=C, prob=weightClass, 
                   response=response, distance=distance, call=ca, terms=mt)	
    class(result) <- 'kknn'
    result
}


# valid=NULL fuer leave one out?
# include in kknn, train.kknn? 
#' @rdname kknn
#' @export
kknn.dist <- function(learn, valid, k = 10, distance = 2) 
{
    m <- dim(learn)[1]
    p <- dim(valid)[1]
    q <- dim(learn)[2]
  
    if(k>m) stop('k must be smaller or equal the number of rows of the training
                 set')
    
    we <- rep(1.0, q)
  
    ord <- order(we * apply(learn, 2, sd), decreasing=TRUE)
  
    learn <- learn[,ord, drop=FALSE]
    valid <- valid[,ord, drop=FALSE]
    
    Euclid <- FALSE
    if(distance==2) Euclid <- TRUE
    if(Euclid) dmtmp <- .C("dmEuclid", as.double(learn), as.double(valid), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm=double(k * p), cl=integer(k * p), k=as.integer(k), 
        as.double(we), PACKAGE='kknn')
    else dmtmp <- .C("dm", as.double(learn), as.double(valid), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm=double(k * p), cl=integer(k * p), k=as.integer(k), 
        as.double(distance),as.double(we), PACKAGE='kknn')
    D <- matrix(dmtmp$dm, nrow = p, ncol = k)
    C <- matrix(dmtmp$cl, nrow = p, ncol = k) + 1L
    list(C, D)
}  


#' @param x an object used to select a method.
#' @param digits minimal number of significant digits.
#' @param ... further arguments passed to or from other methods.
#' @rdname kknn
#' @export
print.kknn <- function(x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
   	cat("Response: ",deparse(x$response),"\n",sep="")
}


#' @rdname kknn
#' @export
summary.kknn <- function(object, ...)
{

	cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
	cat("Response: ",deparse(object$response),"\n",sep="")
	digits <- max(3, getOption("digits") - 3)
	if(object$response!="continuous")print(data.frame(fit=object$fitted.value,
	                                                  prob=object$prob),digits)
	fit <- object$fit
}


#' @rdname kknn
#' @param object a model object for which prediction is desired.
#' @param type defines the output, 'raw' returns the estimates, 'prob' returns
#' a matrix containing the proportions of each class. 
#' @export
predict.kknn <- function(object, type = c("raw", "prob"), ...) 
{ 
    call <- object$call
    extras <- match.call(expand.dots = FALSE)$...
    if (length(extras)) {
        names(extras)[names(extras) == "new.data"] = "test"
        existing <- !is.na(match(names(extras), c("test", "k", "distance",
                           "kernel", "ykernel", "scale", "contrasts")))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
#        if (any(!existing)) {
#              call <- c(as.list(call), extras[!existing])
#              call <- as.call(call)
#        }
        object <- eval(call, object, parent.frame())
    }  
    type <- match.arg(type)
    if(type=="raw") return(object$fit)
    if(type=="prob") return(object$prob)
    return(NULL)
}


#' @importFrom stats formula predict terms
#' @param object a model object for which prediction is desired.
#' @param newdata A data frame in which to look for variables with which to 
#' predict.
#' @rdname train.kknn
#' @export
predict.train.kknn <- function (object, newdata, ...) 
{
    if (missing(newdata)) 
        return(predict(object, ...)) 
    res <- kknn(formula(terms(object)), object$data, newdata, 
        k = object$best.parameters$k, kernel = object$best.parameters$kernel, 
        distance = object$distance)
    return(predict(res, ...))
}




#' Training kknn
#' 
#' Training of kknn method via leave-one-out (\code{train.kknn}) or k-fold
#' (\code{cv.kknn}) cross-validation.
#' 
#' \code{train.kknn} performs leave-one-out cross-validation and is
#' computationally very efficient. \code{cv.kknn} performs k-fold
#' cross-validation and is generally slower and does not yet contain the test of
#' different models yet.
#' 
#' @aliases train.kknn plot.train.kknn print.train.kknn predict.train.kknn
#' summary.train.kknn cv.kknn
#' @param formula A formula object.
#' @param data Matrix or data frame.
#' @param kmax Maximum number of k, if \code{ks} is not specified.
#' @param ks A vector specifying values of k. If not null, this takes
#' precedence over \code{kmax}.
#' @param distance Parameter of Minkowski distance.
#' @param kernel Kernel to use. Possible choices are "rectangular" (which is
#' standard unweighted knn), "triangular", "epanechnikov" (or beta(2,2)),
#' "biweight" (or beta(3,3)), "triweight" (or beta(4,4)), "cos", "inv",
#' "gaussian" and "optimal".
#' @param ykernel Window width of an y-kernel, especially for prediction of
#' ordinal classes.
#' @param scale logical, scale variable to have equal sd.
#' @param contrasts A vector containing the 'unordered' and 'ordered' contrasts
#' to use.
#' @param \dots Further arguments passed to or from other methods.
#' @param kcv Number of partitions for k-fold cross validation.
#' @return \code{train.kknn} returns a list-object of class \code{train.kknn}
#' including the components.  \item{MISCLASS}{Matrix of misclassification
#' errors.} \item{MEAN.ABS}{Matrix of mean absolute errors.}
#' \item{MEAN.SQU}{Matrix of mean squared errors.} \item{fitted.values}{List of
#' predictions for all combinations of kernel and k.}
#' \item{best.parameters}{List containing the best parameter value for kernel
#' and k.} \item{response}{Type of response variable, one of \emph{continuous},
#' \emph{nominal} or \emph{ordinal}.} \item{distance}{Parameter of Minkowski
#' distance.} \item{call}{The matched call.} \item{terms}{The 'terms' object
#' used.}
#' @author Klaus P. Schliep \email{klaus.schliep@@gmail.com}
#' @seealso \code{\link[kknn]{kknn}}
#' @references Hechenbichler K. and Schliep K.P. (2004) \emph{Weighted
#' k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper
#' 399, SFB 386, Ludwig-Maximilians University Munich
#' (\doi{10.5282/ubm/epub.1769})
#' 
#' Hechenbichler K. (2005) \emph{Ensemble-Techniken und ordinale
#' Klassifikation}, PhD-thesis
#' 
#' Samworth, R.J. (2012) \emph{Optimal weighted nearest neighbour classifiers.}
#' Annals of Statistics, 40, 2733-2763. (available from
#' \url{http://www.statslab.cam.ac.uk/~rjs57/Research.html})
#' @keywords classif
#' @examples
#' 
#' library(kknn)
#' \dontrun{
#' data(miete)
#' (train.con <- train.kknn(nmqm ~ wfl + bjkat + zh, data = miete, 
#' 	kmax = 25, kernel = c("rectangular", "triangular", "epanechnikov",
#' 	"gaussian", "rank", "optimal")))
#' plot(train.con)
#' (train.ord <- train.kknn(wflkat ~ nm + bjkat + zh, miete, kmax = 25,
#'  	kernel = c("rectangular", "triangular", "epanechnikov", "gaussian", 
#'  	"rank", "optimal")))
#' plot(train.ord)
#' (train.nom <- train.kknn(zh ~ wfl + bjkat + nmqm, miete, kmax = 25, 
#' 	kernel = c("rectangular", "triangular", "epanechnikov", "gaussian", 
#' 	"rank", "optimal")))
#' plot(train.nom)
#' }
#' data(glass)
#' glass <- glass[,-1]
#' (fit.glass1 <- train.kknn(Type ~ ., glass, kmax = 15, kernel = 
#' 	c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
#' (fit.glass2 <- train.kknn(Type ~ ., glass, kmax = 15, kernel = 
#' 	c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
#' plot(fit.glass1)
#' plot(fit.glass2)
#' 
#' @rdname train.kknn
#' @export train.kknn
train.kknn <- function (formula, data, kmax = 11, ks = NULL, distance = 2, 
                        kernel = "optimal", ykernel = NULL, scale=TRUE, 
    contrasts = c(unordered = "contr.dummy", ordered = "contr.ordinal"), ...) 
{
	if(is.null(ykernel)) ykernel <- 0
    weight.y <- function(l = 1, diff = 0) {
        k <- diff + 1
        result <- matrix(0, l, l)
        diag(result) <- k
        for (i in 1:(k - 1)) {
            for (j in 1:(l - i)) {
                result[j, j + i] <- k - i
                result[j + i, j] <- k - i
            }
        }
        result
    }
    kernel <- match.arg(kernel, c("rectangular", "triangular", "epanechnikov",
                                  "biweight", "triweight", "cos", "inv", 
                                  "gaussian", "rank", "optimal"), TRUE)
    call <- match.call()
    mf <- model.frame(formula, data = data)
    mt <- attr(mf, "terms")

    y <- model.response(mf)
    cl <- model.response(mf)
#    old.contrasts <- getOption("contrasts")
#    options(contrasts = contrasts)
    
    contrasts.arg <- get_contrasts(mt, contrasts)  
    
    mm.data <- model.matrix(mt, mf, contrasts.arg=contrasts.arg)
    
    p <- m <- dim(mm.data)[1]
    q <- dim(mm.data)[2]
    
    d <- sum(attr(mt, "order"))

    if(kmax >= m){
      warning("kmax was to high")
      kmax <- m-1L
    } 
    if (is.null(ks)) {
      ks <- 1:kmax
      nk <- kmax
    } else {
      ks <- sort(ks)
      nk <- length(ks)
      kmax <- max(ks)
    }
    
    r <- length(kernel)
    P <- list(nk * r)


    MISCLASS <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
    MEAN.ABS <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
    MEAN.SQU <- matrix(nrow = nk, ncol = r, dimnames = list(ks, kernel))
    
    ind <- attributes(mm.data)$assign
  
    d.sd <- numeric(length(ind)) + 1
    we <- numeric(length(ind)) + 1

#    for (i in 1:max(ind)) {
#        d.sd[ind == i] = sqrt(mean(diag(cov(as.matrix(mm.data[, ind == i])))))
#        we[ind == i] = 1/sum(ind == i)
#    }

    d.sd <- apply(mm.data, 2, stats::var)
    for (i in unique(ind)){
    	d.sd[ind==i] <- sqrt(mean(d.sd[ind==i]))
    	we[ind==i] <- 1/sum(ind==i)
    	}

    we[d.sd == 0] <- 0
    d.sd[d.sd == 0] <- 1
#    mm.data <- t(t(mm.data)/d.sd) 
 
#    raus 5.3.2013  
#    if(scale) mm.data <- mm.data %*% diag(1/d.sd)         
  	if(scale) mm.data <- sweep(mm.data, 2L, d.sd, "/", check.margin = FALSE) 
    ord <- order(we * apply(mm.data, 2, sd), decreasing=TRUE)
# ordering
    mm.data <- mm.data[, ord, drop=FALSE]  
    we <- we[ord]

    Euclid <- FALSE
    if(distance==2) Euclid <- TRUE
    
    kmax2 <- kmax + 2L
    if(kmax2 > m) kmax2 <- m
    if(Euclid) dmtmp <- .C(dmEuclid, as.double(mm.data), as.double(mm.data), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm = double((kmax2) * p), cl = integer(kmax2 * p), 
        k = as.integer(kmax2), as.double(we))
    else dmtmp <- .C(dm, as.double(mm.data), as.double(mm.data), 
        as.integer(m), as.integer(p), as.integer(q), 
        dm = double(kmax2 * p), cl = integer(kmax2 * p), 
        k = as.integer(kmax2), as.double(distance), 
        as.double(we))
    D <- matrix(dmtmp$dm, nrow = p, ncol = kmax2)
    C <- matrix(dmtmp$cl, nrow = p, ncol = kmax2)
    C <- C + 1
    CL <- matrix(cl[C], nrow = p, ncol = kmax2)  # y statt cl
    D <- D[, -1]
    C <- C[, -1]
    CL <- CL[, -1]
    if (is.ordered(y)) {
        response <- "ordinal"
        lev <- levels(y)
        l <- length(lev)
        weightClass <- matrix(0, m, l)
    }
    if (is.numeric(y)) {
        response <- "continuous"
        weightClass <- NULL
    }
    if (is.factor(y) & !is.ordered(y)) {
        response <- "nominal"
        lev <- levels(y)
        l <- length(lev)
        weightClass <- matrix(0, m, l)
    }

    for (k_i in 1:nk) {
        j <- ks[k_i]
        maxdist <- D[, min(j + 1, ncol(D)) ]
        maxdist[maxdist < 1.0e-06] <- 1.0e-06
        V <- D[, 1:j]/ maxdist # sapply(maxdist, "max", 1e-06)
#        V <- D[, 1:j]/sapply(maxdist, "max", 1e-06)
        V <- pmin(V, 1 - (1e-06))
        V <- pmax(V, 1e-06)
        for (s in 1:r) {
            if (kernel[s] == "rank") 
                W <- (j + 1) - t(apply(as.matrix(V), 1, rank))
            if (kernel[s] == "inv") 
                W <- 1/V
            if (kernel[s] == "rectangular") 
                W <- matrix(1, nrow = m, ncol = j)
            if (kernel[s] == "triangular") 
                W <- 1 - V
            if (kernel[s] == "epanechnikov") 
                W <- 0.75 * (1 - V^2)
            if (kernel[s] == "biweight") 
                W <- dbeta((V + 1)/2, 3, 3)
            if (kernel[s] == "triweight") 
                W <- dbeta((V + 1)/2, 4, 4)
            if (kernel[s] == "cos") 
                W <- cos(V * pi/2)
            if (kernel[s] == "gaussian") {
            	v <- j + 1
         	    alpha <- 1/(2 * v)
             	qua <- abs(qnorm(alpha))
             	W <- V*qua
            	W <- apply(as.matrix(W), 2, dnorm)
            }
            if (kernel[s] == "optimal") {
                W <- rep(optKernel(j,d), each=m)
            }
            W <- matrix(W, m, j)
            if (response != "continuous") {
                for (i in 1:l) {
                  weightClass[, i] <- rowSums(W * (matrix(CL[, 
                    1:j], m, j) == lev[i]))
                }
                weightClass <- weightClass/rowSums(weightClass)
                colnames(weightClass) <- lev
            }
            if (response == "ordinal") {
                blub <- length(lev)
                weightClass <- weightClass %*% weight.y(blub, ykernel)
                weightClass <- weightClass/rowSums(weightClass)
                weightClass <- t(apply(weightClass, 1, cumsum))
                colnames(weightClass) <- lev
                fit <- numeric(m)             
				fit <- ((l+1)-(weightClass >= 0.5)%*%(numeric(l)+1))
                fit <- ordered(fit, levels = 1:l, labels = lev)
            }
            if (response == "nominal") {
				        lwc <- length(weightClass)
                fit <- apply(weightClass, 1, order, decreasing = TRUE)[1, ]
                fit <- factor(fit, levels = 1:l, labels = lev)
            }
            if (response == "continuous") {
#      fit <- rowSums(W * (matrix(CL[, 1:j], m, j)))
#      /sapply(rowSums(matrix(W, m, j)), "max", 1e-06)
                fit <- rowSums(W * (matrix(CL[, 1:j], m, j))) / 
                         pmax(rowSums(matrix(W, m, j)), 1e-06)
                weightClass <- fit
            }
            attr(fit, "kernel") <- kernel[s]
            attr(fit, "k") <- j
            P[[k_i + (s - 1) * nk]] <- fit

        }
    }

    for (k_i in 1:nk) {
        j <- ks[k_i]
        for (s in 1:r) {
            if (is.factor(y)) 
                MISCLASS[k_i, s] <- sum(y != P[[k_i + (s - 1) * nk]])/m
            if (is.numeric(y) | is.ordered(y)) 
                MEAN.ABS[k_i, s] <- sum(abs(as.numeric(y) - as.numeric(P[[k_i + 
                  (s - 1) * nk]])))/m
            if (is.numeric(y) | is.ordered(y)) 
                MEAN.SQU[k_i, s] <- sum((as.numeric(y) - as.numeric(P[[k_i + 
                  (s - 1) * nk]]))^2)/m
        }
    }
    if (response == "nominal") 
        best <- which(MISCLASS == min(MISCLASS), arr.ind = TRUE)
    if (response == "ordinal") 
        best <- which(MEAN.ABS == min(MEAN.ABS), arr.ind = TRUE)
    if (response == "continuous") 
        best <- which(MEAN.SQU == min(MEAN.SQU), arr.ind = TRUE)
    best.parameters <- list(kernel = kernel[best[1, 2]], k = ks[best[1, 1]])

#    options('contrasts'=old.contrasts)
    
    result <- list(MISCLASS = MISCLASS, MEAN.ABS = MEAN.ABS, 
                   MEAN.SQU= MEAN.SQU, fitted.values = P, 
                   best.parameters = best.parameters, response = response, 
                   distance = distance, call = call, terms = mt, data = data)
    class(result) <- c("train.kknn", "kknn")
    result
}


#' @rdname train.kknn
#' @export
print.train.kknn <- function(x, ...)
{
	cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	cat("Type of response variable: ", x$response,"\n", sep = "")
	if(x$response=='continuous'){cat("minimal mean absolute error: ", 
	                                 min(x$MEAN.ABS),"\n", sep = "")
		cat("Minimal mean squared error: ",min(x$MEAN.SQU),"\n", sep = "")}
	if(x$response=='nominal'){
		cat("Minimal misclassification: ",min(x$MISCLASS),"\n", sep = "")}
	if(x$response=='ordinal'){cat("minimal mean absolute error: ",min(x$MEAN.ABS),
	                              "\n", sep = "")
		cat("Minimal mean squared error: ",min(x$MEAN.SQU),"\n", sep = "")
		cat("Minimal misclassification: ",min(x$MISCLASS),"\n", sep = "")
		}	
	cat("Best kernel: ", x$best$kernel,"\n", sep = "")
	cat("Best k: ", x$best$k,"\n", sep = "")	
}


#' @rdname train.kknn
#' @export
summary.train.kknn <- function(object, ...)
{
	cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
	cat("Type of response variable: ", object$response,"\n", sep = "")
	if(object$response=='continuous'){cat("minimal mean absolute error: ",
	                                      min(object$MEAN.ABS),"\n", sep = "")
		cat("Minimal mean squared error: ",min(object$MEAN.SQU),"\n", sep = "")}
	if(object$response=='nominal'){
		cat("Minimal misclassification: ",min(object$MISCLASS),"\n", sep = "")}
	if(object$response=='ordinal'){cat("minimal mean absolute error: ",
	                                   min(object$MEAN.ABS),"\n", sep = "")
		cat("Minimal mean squared error: ",min(object$MEAN.SQU),"\n", sep = "")
		cat("Minimal misclassification: ",min(object$MISCLASS),"\n", sep = "")
		}	
	cat("Best kernel: ", object$best$kernel,"\n", sep = "")
	cat("Best k: ", object$best$k,"\n", sep = "")	
}


#' @importFrom graphics matplot legend par xinch yinch
#' @param x an object of class \code{train.kknn}
#' @rdname train.kknn
#' @export
plot.train.kknn <-function(x,...){
	if(x$response=='continuous'){		
		legend.text <- colnames(x$MEAN.ABS)
		m <- 1:length(colnames(x$MEAN.ABS))
		matplot(x = as.integer(rownames(x$MEAN.SQU)),
		        y = x$MEAN.SQU, xlab="k", ylab="mean squared error",pch = m,...)
		xy <- par("usr")
		legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text, 
		       xjust = 1, yjust = 1,col=m,pch=m)
		}
	if(x$response=='ordinal'){
		legend.text <- colnames(x$MISCLASS)
		m <- 1:length(colnames(x$MISCLASS))
		matplot(x = as.integer(rownames(x$MEAN.ABS)),
		        y = x$MEAN.ABS, xlab="k", ylab="mean absolute error",pch = m,...)
		xy <- par("usr")
		legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text, 
		       xjust = 1, yjust = 1,col=m,pch=m)
		}
	if(x$response=='nominal'){
		legend.text <- colnames(x$MISCLASS)
		m <- 1:length(colnames(x$MISCLASS))
		matplot(x = as.integer(rownames(x$MISCLASS)),
		        y = x$MISCLASS, xlab="k", ylab="misclassification",pch = m,...)
		xy <- par("usr")
		legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text, 
		       xjust = 1, yjust = 1,col=m,pch=m)
		}	
}


#' @rdname train.kknn
#' @export
cv.kknn <- function(formula, data, kcv = 10, ...)
{
  mf <- model.frame(formula, data=data) 
  # terms(formula, data = data) keine kopie der Daten?
  y <- model.response(mf)                 
  l <- length(y)    # nrow(data)                  
  val<-sample(kcv, size=l, replace=TRUE) 
  yhat <- numeric(l)
  for(i in 1:kcv){
    m <- dim(data)[1]
    learn <- data[val!=i,]
    valid <- data[val==i,]
    fit <- kknn(formula , learn, valid, ...)
    yhat[val==i] <- predict(fit)
  }  
  if(is.factor(y)) MISCLASS <- sum(y != yhat)/l
  if(is.numeric(y) | is.ordered(y)) MEAN.ABS <- sum(abs(as.numeric(y) - 
                                                          as.numeric(yhat)))/l
  if(is.numeric(y) | is.ordered(y)) MEAN.SQU <- sum((as.numeric(y) - 
                                                       as.numeric(yhat))^2)/l
  if(is.numeric(y)) result <- c(MEAN.ABS, MEAN.SQU)
  if(is.ordered(y)) result <- c(MISCLASS, MEAN.ABS, MEAN.SQU)
  if(is.factor(y) & !is.ordered(y)) result<-MISCLASS 
  list(cbind(y=y, yhat=yhat), result)
}


prepare.Discrete <- function(data){
  if(inherits(data, "factor")) return(as.matrix(unclass(data)))
  if(inherits(data, "data.frame")) 
    return(as.matrix(data.frame(lapply(data,unclass))))
}
