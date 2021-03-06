% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arpr.R
\name{arpr}
\alias{arpr}
\title{At-risk-of-poverty rate}
\usage{
arpr(dataset, ipuc = "ipuc", hhcsw = "DB090", hhsize = "HX040",
  arpt.value = NULL, ci = NULL, rep = 1000, verbose = FALSE)
}
\arguments{
\item{dataset}{a data.frame containing the variables.}

\item{ipuc}{a character string indicating the variable name of the income per unit of consumption. Default is "ipuc".}

\item{hhcsw}{a character string indicating the variable name of the household cross-sectional weight. Default is "DB090".}

\item{hhsize}{a character string indicating the variable name of the household size. Default is "HX040".}

\item{arpt.value}{the at-risk-of-poverty threshold to be used  (see arpt). Default is NULL which calculates arpt with default parameters.}

\item{ci}{a scalar or vector containing the confidence level(s) of the required interval(s). Default does not calculate the confidence interval.}

\item{rep}{a number to make the confidence interval using boostrap technique.}

\item{verbose}{logical; if TRUE the confidence interval is plotted.}
}
\value{
The value of the at-risk-of-poverty rate.
}
\description{
Estimates the poverty rate which is defined as the share of people with an equivalised disposable income below the at-risk-of-poverty threshold.
}
\details{
The equivalised disposable income is calculated using the standard equivalence scale (called the modified OECD scale) recommended by Eurostat. The parametric scale of Buhmann et al. (1988) can also be used. The default is the modified OECD scale  (see setupDataset).
}
\examples{
data(eusilc2)
ATdataset <- setupDataset(eusilc2, country = "AT")
arpr(ATdataset,arpt.value = arpt(ATdataset))

}
\references{
B. Buhmann et al. (1988) Equivalence scales, well-being, inequality and poverty: sensitivity estimates across ten countries using the Luxembourg Income Study (LIS) database, Review of Income and Wealth, 34, 115--142.

\url{http://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:At-risk-of-poverty_rate}
}
\seealso{
arpt, setupDataset
}
\author{
A. Berihuete, C.D. Ramos and M.A. Sordo
}
