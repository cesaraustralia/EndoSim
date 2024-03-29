% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_weibull.R
\name{fit_weibull}
\alias{fit_weibull}
\title{Fit Weibull curve for fecundity}
\usage{
fit_weibull(K, iota, gamma, eta)
}
\arguments{
\item{K}{fecundity rate scalar}

\item{iota}{shape of fecundity curve}

\item{gamma}{scale of fecundity curve}

\item{eta}{locations of fecundity curve}
}
\value{
function that accepts x and returns y based on fitted model
}
\description{
Fits a function that returns daily fecundity based on cohort adult age
}
\details{
Fits a curve using the Weibull distribution to describe how daily fecundity changes with adult age:
\deqn{
K \cdot \frac{\iota}{\gamma} \cdot  \left( \frac{\text{age} - \eta}{\gamma} \right)^{\iota - 1} \cdot e^{-\left( \frac{\text{age} - \eta}{\gamma} \right)^\iota}
}
}
\examples{
my_func <- EndoSim:::fit_weibull(58, 1.885, 5.953, 0)
plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
}
\references{
Weibull, W., A statistical distribution function of wide applicability. Journal of Applied Mechanics, 18, 293-297 (1951)
}
\seealso{
\code{\link[=fit_bannerman]{fit_bannerman()}}, \code{\link[=fit_null]{fit_null()}}, \code{\link[=fit_quadratic]{fit_quadratic()}}
}
\keyword{internal}
