% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kknn-package.R
\docType{data}
\name{glass}
\alias{glass}
\title{Glass Identification Database}
\format{
A data frame with 214 observations on the following 11 variables.
\describe{ \item{Id}{Id number.} \item{RI}{Refractive index.}
\item{Na}{Sodium (unit measurement: weight percent in corresponding oxide,
as are attributes 4-10).} \item{Mg}{Magnesium.} \item{Al}{Aluminum.}
\item{Si}{Silicon.} \item{K}{Potassium.} \item{Ca}{Calcium.}
\item{Ba}{Barium.} \item{Fe}{Iron.} \item{Type}{Type of glass: (class
attribute) \cr \code{1} building windows float processed \cr \code{2}
building windows non float processed \cr \code{3} vehicle windows float
processed \cr \code{4} vehicle windows non float processed (none in this
database) \cr \code{5} containers \cr \code{6} tableware \cr \code{7}
headlamps } }
}
\source{
\itemize{ \item Creator: B. German, Central Research Establishment,
Home Office Forensic Science Service, Aldermaston, Reading, Berkshire RG7
4PN \item Donor: Vina Spiehler, Ph.D., DABFT, Diagnostic Products
Corporation } The data have been taken from the UCI Machine Learning
Database Repository \cr
\url{https://archive.ics.uci.edu/} \cr and were
converted to R format by \email{klaus.schliep@gmail.com}.
}
\description{
A data frame with 214 observations, where the problem is to predict the type
of glass in terms of their oxide content (i.e. Na, Fe, K, etc). The study of
classification of types of glass was motivated by criminological
investigation.  At the scene of the crime, the glass left can be used as
evidence... if it is correctly identified!
}
\examples{

data(glass)
str(glass)

}
\keyword{datasets}
