

#' Glass Identification Database
#' 
#' A data frame with 214 observations, where the problem is to predict the type
#' of glass in terms of their oxide content (i.e. Na, Fe, K, etc). The study of
#' classification of types of glass was motivated by criminological
#' investigation.  At the scene of the crime, the glass left can be used as
#' evidence... if it is correctly identified!
#' 
#' 
#' @name glass
#' @docType data
#' @format A data frame with 214 observations on the following 11 variables.
#' \describe{ \item{Id}{Id number.} \item{RI}{Refractive index.}
#' \item{Na}{Sodium (unit measurement: weight percent in corresponding oxide,
#' as are attributes 4-10).} \item{Mg}{Magnesium.} \item{Al}{Aluminum.}
#' \item{Si}{Silicon.} \item{K}{Potassium.} \item{Ca}{Calcium.}
#' \item{Ba}{Barium.} \item{Fe}{Iron.} \item{Type}{Type of glass: (class
#' attribute) \cr \code{1} building windows float processed \cr \code{2}
#' building windows non float processed \cr \code{3} vehicle windows float
#' processed \cr \code{4} vehicle windows non float processed (none in this
#' database) \cr \code{5} containers \cr \code{6} tableware \cr \code{7}
#' headlamps } }
#' @source \itemize{ \item Creator: B. German, Central Research Establishment,
#' Home Office Forensic Science Service, Aldermaston, Reading, Berkshire RG7
#' 4PN \item Donor: Vina Spiehler, Ph.D., DABFT, Diagnostic Products
#' Corporation } The data have been taken from the UCI Machine Learning
#' Database Repository \cr
#' \url{https://archive.ics.uci.edu/} \cr and were
#' converted to R format by \email{klaus.schliep@gmail.com}.
#' @keywords datasets
#' @examples
#' 
#' data(glass)
#' str(glass)
#' 
NULL





#' Johns Hopkins University Ionosphere Database
#' 
#' This radar data was collected by a system in Goose Bay, Labrador.  This
#' system consists of a phased array of 16 high-frequency antennas with a total
#' transmitted power on the order of 6.4 kilowatts.  See the paper for more
#' details.  The targets were free electrons in the ionosphere. "Good" radar
#' returns are those showing evidence of some type of structure in the
#' ionosphere.  "Bad" returns are those that do not; their signals pass through
#' the ionosphere.
#' 
#' Received signals were processed using an autocorrelation function whose
#' arguments are the time of a pulse and the pulse number.  There were 17 pulse
#' numbers for the Goose Bay system.  Instances in this database are described
#' by 2 attributes per pulse number, corresponding to the complex values
#' returned by the function resulting from the complex electromagnetic signal.
#' 
#' 
#' @name ionosphere
#' @docType data
#' @format A data frame with 351 observations on the following 35 variables.
#' The first 34 continuous covariables are used for the prediction. The 35th
#' attribute is either \code{g} ("good") or \code{b} ("bad") according to the
#' definition summarized above. This is a binary classification task.
#' @source % \itemize{\item Vince Sigillito (vgs@aplcen.apl.jhu.edu), Space
#' Physics Group, Applied Physics Laboratory, Johns Hopkins University, Johns
#' Hopkins Road, Laurel, MD 20723
#' 
#' The data have been taken from the UCI Machine Learning Database Repository
#' \cr \url{https://archive.ics.uci.edu/}\cr and were
#' converted to R format by \email{klaus.schliep@gmail.com }.
#' @keywords datasets
#' @examples
#' 
#' data(ionosphere)
#' 
NULL





#' Weighted k-Nearest Neighbors Classification and Clustering
#' 
#' Weighted k-Nearest Neighbors Classification, Regression and spectral
#' Clustering
#' 
#' The complete list of functions can be displayed with \code{library(help =
#' kknn)}.
#' 
#' 
#' @name kknn-package
#' @docType package
#' @author Klaus Schliep \cr Klaus Hechenbichler
#' 
#' Maintainer: Klaus Schliep <klaus.schliep@@gmail.com>
#' @references Hechenbichler K. and Schliep K.P. (2004) \emph{Weighted
#' k-Nearest-Neighbor Techniques and Ordinal Classification}, Discussion Paper
#' 399, SFB 386, Ludwig-Maximilians University Munich
#' (\doi{10.5282/ubm/epub.1769})
#' 
#' Hechenbichler K. (2005) \emph{Ensemble-Techniken und ordinale
#' Klassifikation}, PhD-thesis
#' @useDynLib kknn, .registration = TRUE
#' @keywords internal
"_PACKAGE"





#' Munich Rent Standard Database (1994)
#' 
#' Many german cities compose so-called rent standards to make a decision
#' making instrument available to tenants, landlords, renting advisory boards
#' and experts. The rent standards are used in particular for the determination
#' of the local comparative rent (i.e. net rent as a function of household
#' size, equipment, year of construction, etc.). For the composition of the
#' rent standards, a representative random sample is drawn from all relevant
#' households, and the interesting data are determined by interviewers by means
#' of questionnaires. The dataset contains the data of 1082 households
#' interviewed for the munich rent standard 1994.
#' 
#' 
#' @name miete
#' @docType data
#' @format A data frame with 1082 observations on the following 18 variables.
#' \describe{ \item{nm}{Net rent in DM.} \item{wfl}{Floor space in sqm.}
#' \item{bj}{Year of construction.} \item{bad0}{Bathroom in apartment?\cr 1 :
#' no\cr 0 : yes } \item{zh}{Central heating?\cr 1 : yes\cr 0 : no }
#' \item{ww0}{Hot water supply?\cr 1 : no\cr 0 : yes }
#' 
#' \item{badkach}{Tiled bathroom?\cr 1 : yes\cr 0 : no } \item{fenster}{Window
#' type:\cr 1 : plain windows\cr 0 : state-of-the-art windows }
#' \item{kueche}{Kitchen type\cr 1 : well equipped kitchen\cr 0 : plain kitchen
#' } \item{mvdauer}{Lease duration in years.} \item{bjkat}{Age category of the
#' building (bj categorized)\cr 1 : built before 1919\cr 2 : built between 1919
#' and 1948\cr 3 : built between 1949 and 1965\cr 4 : built between 1966 and
#' 1977\cr 5 : built between 1978 and 1983\cr 6 : built after 1983 }
#' \item{wflkat}{Floor space category (wfl categorized):\cr 1 : less than 50
#' sqm\cr 2 : between 51 sqm and 80 sqm\cr 3 : at least 81 sqm }
#' \item{nmqm}{Net rent per sqm.} \item{rooms}{Number of rooms in household.}
#' 
#' \item{nmkat}{Net rent category (nm categorized):\cr 1 : less than 500 DM\cr
#' 2 : between 500 DM and 675 DM\cr 3 : between 675 DM and 850 DM\cr 4 :
#' between 850 DM and 1150 DM\cr 5 : at least 1150 DM } \item{adr}{Address
#' type:\cr 1 : bad\cr 2 : average\cr 3 : good }
#' 
#' \item{wohn}{Residential type:\cr 1 : bad\cr 2 : average\cr 3 : good } }
#' @source Fahrmeir, L., Kuenstler, R., Pigeot, I. und Tutz, G. (1997):
#' \emph{Statistik: der Weg zur Datenanalyse}, Springer, Berlin.
## \url{https://www.stat.lmu.de/service/datenarchiv}
#' 
#' The data were converted to R format by \email{klaus.schliep@gmail.com}.
#' @keywords datasets
#' @examples
#' 
#' data(miete)
#' str(miete)
#' 
NULL



