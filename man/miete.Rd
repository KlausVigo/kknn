\name{miete}
\alias{miete}
\docType{data}
\title{Munich Rent Standard Database (1994)}
\description{
Many german cities compose so-called rent standards to make a decision making instrument 
available to tenants, landlords, renting advisory boards and experts. The rent standards 
are used in particular for the determination of the local comparative rent (i.e. net rent 
as a function of household size, equipment, year of construction, etc.). For the composition
of the rent standards, a representative random sample is drawn from all relevant households, 
and the interesting data are determined by interviewers by means of questionnaires. The 
dataset contains the data of 1082 households interviewed for the munich rent standard 1994.  
}
\usage{data(miete)}
\format{
A data frame with 1082 observations on the following 18 variables.
\describe{
    \item{nm}{Net rent in DM.}
    \item{wfl}{Floor space in sqm.}
    \item{bj}{Year of construction.}
    \item{bad0}{Bathroom in apartment?\cr
1 : no\cr
0 : yes }
    \item{zh}{Central heating?\cr
1 : yes\cr
0 : no 
}
    \item{ww0}{Hot water supply?\cr
1 : no\cr
0 : yes  
}

    \item{badkach}{Tiled bathroom?\cr
1 : yes\cr
0 : no  
}
    \item{fenster}{Window type:\cr
1 : plain windows\cr
0 : state-of-the-art windows  
}
    \item{kueche}{Kitchen type\cr
1 : well equipped kitchen\cr
0 : plain kitchen  
}
    \item{mvdauer}{Lease duration in years.}
    \item{bjkat}{Age category of the building (bj categorized)\cr
1 : built before 1919\cr
2 : built between 1919 and 1948\cr
3 : built between 1949 and 1965\cr
4 : built between 1966 and 1977\cr
5 : built between 1978 and 1983\cr
6 : built after 1983  
}
    \item{wflkat}{Floor space category (wfl categorized):\cr 
1 : less than 50 sqm\cr 
2 : between 51 sqm and 80 sqm\cr
3 : at least 81 sqm  
}
    \item{nmqm}{Net rent per sqm.}
    \item{rooms}{Number of rooms in household.}

  \item{nmkat}{Net rent category (nm categorized):\cr      
1 : less than 500 DM\cr 
2 : between 500 DM and 675 DM\cr
3 : between 675 DM and 850 DM\cr
4 : between 850 DM and 1150 DM\cr
5 : at least 1150 DM   
}    
    \item{adr}{Address type:\cr
1 : bad\cr
2 : average\cr
3 : good
}


    \item{wohn}{Residential type:\cr
1 : bad\cr
2 : average\cr
3 : good
}
}
}
\source{
Fahrmeir, L., Kuenstler, R., Pigeot, I. und Tutz, G. (1997): \emph{Statistik: der Weg zur Datenanalyse}, Springer, Berlin. 
\url{http://www.stat.uni-muenchen.de/service/datenarchiv}

The data were converted to R format by \email{klaus.schliep@gmail.com}.
}
\examples{
data(miete)
str(miete)
}
\keyword{datasets}
