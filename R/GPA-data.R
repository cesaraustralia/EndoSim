#' GPA species information
#'
#' A species object defining the life history and function
#' for green peach aphid (\emph{Myzus persicae}).
#' Can be used to run the endosymbiont model on GPA.\cr
#' \cr
#' \code{f_growth} fitted using [fit_custom] with data from Whalon & Smilowitz 1979.\cr
#' \code{f_loss} fitted using [fit_sigmoid] with data from Maling et al. 2008.\cr
#' \code{f_suscept} fitted using [fit_custom] with data from Thackray et al. 2009.\cr
#' \code{f_inoc} fitted using [fit_quadratic] with data from Thackray et al. 2009.\cr
#' \code{f_alate} fitted using [fit_custom] with data from Thackray et al. 2009.\cr
#'
#' @docType data
#'
#' @usage data(GPA)
#'
#' @format An object of class \code{species}; see [species].
#'
#' @keywords datasets
#'
#' @references 
#' Maling, T., Diggle, A.J., Thackray, D.J., Siddique, K.H.M., Jones, R.A.C., An epidemiological model for externally sourced vector-borne viruses applied to \emph{Bean yellow mosaic virus} in lupin crops in a Mediterranean-type environment. Phytopathology, 98, 1280–1290 (2008)
#' 
#' Thackray, D.J., Diggle, A.J., Jones, R.A.C., BYDV PREDICTOR: a simulation model to predict aphid arrival, episdemics of \emph{Barley yellow dwarf virus} and yield loss in wheat crops in a Mediterranean-type environment. Plant Pathology, 58, 186–202 (2009)]
#' 
#' Whalon, M.E., Smilowitz, Z., Temperature-dependent model for predicting field populations of green peach aphid, \emph{Myzus persicae}. The Canadian Entomoligst, 111, 1025–1032 (1979)
#' 


"GPA"