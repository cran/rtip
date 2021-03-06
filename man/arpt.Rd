% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arpt.R
\name{arpt}
\alias{arpt}
\title{At-risk-of-poverty threshold}
\usage{
arpt(dataset, ipuc = "ipuc", hhcsw = "DB090", hhsize = "HX040",
  pz = 0.6, ci = NULL, rep = 1000, verbose = FALSE)
}
\arguments{
\item{dataset}{a data.frame containing the variables.}

\item{ipuc}{a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".}

\item{hhcsw}{a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".}

\item{hhsize}{a character string indicating the variable name of the household size. Default is "HX040".}

\item{pz}{a number between 0 and 1 which represents the percentage to be used to calculate the at-risk-of-poverty threshold. The default is 0.6.}

\item{ci}{a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.}

\item{rep}{a number to do the confidence interval using boostrap technique.}

\item{verbose}{logical; if TRUE the confidence interval is plotted.}
}
\value{
The value of the at-risk-of-poverty threshold.
}
\description{
Estimates the at-risk-of-poverty threshold which is set at 60 percent of the median equivalised disposable income using the standard definition.
}
\details{
The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al.(1988) can also be used. The default is the modified OECD scale  (see setupDataset).
}
\examples{
data(eusilc2)
ATdataset <- setupDataset(eusilc2, country = "AT")
arpt(ATdataset)
}
\references{
B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.

\url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
}
\seealso{
setupDataset
}
\author{
A. Berihuete, C.D. Ramos and M.A. Sordo
}
