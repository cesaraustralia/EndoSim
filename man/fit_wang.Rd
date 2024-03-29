% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_wang.R
\name{fit_wang}
\alias{fit_wang}
\title{Fit Wang model for development}
\usage{
fit_wang(H, r, Topt, Tupper, Tlower, bound)
}
\arguments{
\item{H}{development rate scalar}

\item{r}{development rate scalar}

\item{Topt}{optimum development temperature}

\item{Tupper}{upper development threshold}

\item{Tlower}{lower development threshold}

\item{bound}{boundary width of thresholds}
}
\value{
function that accepts x and returns y based on fitted model
}
\description{
Fits a function that returns aphid nymph development rate based on daily temperature
}
\details{
A development rate function to pass on to the Endosymbiont model.
Fits a curve using the Wang model (Wang et al. 1982):
\deqn{
\left(\frac{H}{{1 + e^{-r \cdot (T - T_{\text{opt}})}}}\right) \cdot \left(1 - e^{- \frac{T - T_{\text{lower}}}{\text{bound}}}\right) \cdot \left(1 - e^{- \frac{T_{\text{upper}} - T}{\text{bound}}}\right)
}
}
\examples{
my_func <- EndoSim:::fit_wang(24.1, 0.152, 18.09, 32.79, 2.84, 1.8977)
plot(seq(0, 50, by = 0.1), my_func(seq(0, 50, by = 0.1)), type = "l")
}
\references{
Wang, R.S., Lan, Z.X., Ting, Y.C., Studies on mathematical models of the relationships between insect development and temperature. Acta Ecologica Sinica, 2, 47-57 (1982)
}
\seealso{
\code{\link[=fit_lactin]{fit_lactin()}}, \code{\link[=fit_null]{fit_null()}}
}
\keyword{internal}
