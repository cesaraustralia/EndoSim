#' Parasitoid wasp species information
#'
#' A parasitoid object defining the life history
#' for green peach aphid (\emph{Diaeretiella rapae}).
#' Can be used to run the endosymbiont model on GPA.\cr
#' \cr
#' \code{fun_dev_para} fitted using [fit_lactin] with parameters from Barton et al. 2021 \cr
#' \code{fun_para_scal} fitted using [fit_null] to return 0.83 maximum viability based on Barton et al. 2021 \cr
#' \code{fun_attack} fitted using [fit_thirdpoly] with parameters from Barton et al. 2021 \cr
#' \code{fun_handling} fitted using [fit_thirdpoly] with parameters from Barton et al. 2021 \cr
#' \code{suc_stage} set to 2nd and 3rd instars based on Barton et al. 2021 \cr
#' \code{introduction_date} is 2022-04-20, five days after R+ aphid release \cr
#' \code{introduction_n} is 300 individuals \cr
#'
#' @docType data
#'
#' @usage data(DR)
#'
#' @format An object of class \code{parasitoid}; see [parasitoid-class].
#'
#' @keywords datasets
#'
#' @references 
#' Barton, M., Parry, H., Ward, S., Hoffmann, A.A., Umina, P.A., van Helden, M., Macfadyen, S. Forecasting impacts of biological control under future climates: mechanistic modelling of an aphid pest and a parasitic wasp. Ecological Modelling, 457, 109679 (2021)
#' 

"DR"