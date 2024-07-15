#' GPA species information
#'
#' A pest object defining the life history
#' for green peach aphid (\emph{Myzus persicae}).
#' Can be used to run the endosymbiont model on GPA.\cr
#' \cr
#' \code{bg_loss} derived from Barton et al. 2021 \cr
#' \code{alate_penalty} derived from Barton et al. 2021 \cr
#' \code{apterae_walk} Placeholder \cr
#' \code{alate_flight} Placeholder \cr
#' \code{fun_dev_apt} fitted using [fit_wang] with parameters from Barton et al. 2021 \cr
#' \code{fun_dev_ala} fitted using [fit_wang] with parameters from Barton et al. 2021 \cr
#' \code{fun_imi_neg} fitted using [fit_imi_barton] to describe constant immigration of 50 adults from background population \cr
#' \code{fun_imi_pos} fitted using [fit_null] to describe no new introductions of R+ adult \cr
#' \code{fun_emi} fitted using [fit_emi_thack] to describe a constant emigration rate based on Thackray et al. 2004 \cr
#' \code{fun_temp_loss} fitted using [fit_supergaus] with parameters from Barton et al. 2021 \cr
#' \code{fun_rainfall_loss} fitted using [fit_rainfall_thack] with parameters from Barton et al. 2021 \cr
#' \code{fun_sen_loss} fitted using [fit_gompertz] with parameters from Barton et al. 2021 \cr
#' \code{fun_dens_fecund} fitted using [fit_bannerman] with parameters from Barton et al. 2021 \cr
#' \code{fun_temp_fecund} fitted using [fit_quadratic] with parameters from Barton et al. 2021 \cr
#' \code{fun_age_fecund} fitted using [fit_weibull] with parameters from Barton et al. 2021 \cr
#' \code{fun_alate_prod} fitted using [fit_alate] with parameters from Barton et al. 2021 \cr
#' \code{fun_damage} fitted using [fit_null] to be fixed to zero (no direct feeding damage) \cr
#'
#' @docType data
#'
#' @usage data(GPA)
#'
#' @format An object of class \code{pest}; see [pest-class].
#'
#' @keywords datasets
#'
#' @references 
#' Barton, M., Parry, H., Ward, S., Hoffmann, A.A., Umina, P.A., van Helden, M., Macfadyen, S. Forecasting impacts of biological control under future climates: mechanistic modelling of an aphid pest and a parasitic wasp. Ecological Modelling, 457, 109679 (2021)
#' 
#' Thackray, D.J., Diggle, A.J., Berlandier, F.A., Jones, R.A.C., Forecasting aphid outbreaks and epidemics of Cucumber mosaic virus in lupin crops in a Mediterranean-type environment. Virus Research, 100, 67-82 (2004)
#' 

"GPA"