% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_imi_barton.R
\name{fit_imi_barton}
\alias{fit_imi_barton}
\title{Fit model for immigration}
\usage{
fit_imi_barton()
}
\arguments{
\item{t}{time step}
}
\value{
number of immigrating adults
}
\description{
Fits a function that returns number of immigrating adults
}
\details{
A base immigration rate of 5 adult alates from a background population
}
\examples{
my_func <- EndoSim:::fit_imi_barton()
my_func(runif(10, 1, 100))
}
\references{
Barton, M., Parry, H., Ward, S., Hoffmann, A.A., Umina, P.A., van Helden, M., Macfadyen, S. Forecasting impacts of biological control under future climates: mechanistic modelling of an aphid pest and a parasitic wasp. Ecological Modelling, 457, 109679 (2021)
}
\seealso{
\code{\link[=fit_null]{fit_null()}}
}
\keyword{internal}
