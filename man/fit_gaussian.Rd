% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_gaussian.R
\name{fit_gaussian}
\alias{fit_gaussian}
\title{Fit gaussian curve}
\usage{
fit_gaussian(rmax, topt, a)
}
\arguments{
\item{rmax}{maxmimum rate at optimum temperature}

\item{topt}{optimum reproductive temperature}

\item{a}{width parameter}
}
\value{
function that accepts x and returns y based on fitted model
}
\description{
Fits a function that returns value based on daily temperature
}
\details{
Fits a Gaussian curve to produce a temperature scalar ranging from 0 to rmax:
\deqn{
  r_{max} \cdot e(-0.5 \cdot (\frac{|x - t_{opt}|}{a}))^2)
}
}
\examples{
my_func <- EndoSim:::fit_gaussian(1, 25, 3)
plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
}
\seealso{
\code{\link[=fit_custom]{fit_custom()}}, \code{\link[=fit_null]{fit_null()}}, \code{\link[=fit_quadratic]{fit_quadratic()}}, \code{\link[=fit_rezende]{fit_rezende()}}
}
\keyword{internal}
