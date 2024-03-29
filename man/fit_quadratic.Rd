% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_quadratic.R
\name{fit_quadratic}
\alias{fit_quadratic}
\title{Fit quadratic curve for fecundity}
\usage{
fit_quadratic(a, b, topt)
}
\arguments{
\item{a}{coefficient 1}

\item{b}{coefficient 2}

\item{topt}{optimum reproductive temperature}
}
\value{
function that accepts x and returns y based on fitted model
}
\description{
Fits a function that returns scaled daily fecundity based on daily temperature
}
\details{
Fits a scaled quadratic curve to produce a temperature scalar ranging from 0 to 1:
\deqn{
  \frac{a \cdot x^2 + b \cdot x}{a \cdot T_{opt}^2 + b \cdot T_{opt}}
}
}
\examples{
my_func <- EndoSim:::fit_quadratic(-0.7611, 31.9847, 25)
plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
}
\seealso{
\code{\link[=fit_bannerman]{fit_bannerman()}}, \code{\link[=fit_null]{fit_null()}}, \code{\link[=fit_weibull]{fit_weibull()}}
}
\keyword{internal}
