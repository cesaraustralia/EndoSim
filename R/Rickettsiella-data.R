#' Rickettsiella endosymbiont information
#'
#' An endosym object defining the life history
#' for (\emph{Rickettsiella}).
#' Can be used to run the endosymbiont model on GPA.\cr
#' \cr
#' \code{fitness_cost} is 0.5 based on Gu et al. 2023 \cr
#' \code{fun_trans_eff} fitted using [fit_quadratic] to peak around 20°C based Gu et al. 2023 \cr
#' \code{fun_susc} fitted using [fit_quadratic] to peak around 20°C based Gu et al. 2023 \cr
#' \code{introduction_date} is 2022-04-17, a week after crop emergence \cr
#' \code{introduction_n} is 300 individuals \cr
#'
#' @docType data
#'
#' @usage data(Rickettsiella)
#'
#' @format An object of class \code{endosym}; see [endosym-class].
#'
#' @keywords datasets
#'
#' @references 
#' Gu, X., Ross, P.A., Gill, A., Yang, Q., Ansermin, E., Sharma, S., Soleimannejad, S., Sharma, K., Callahan, A., Brown, C., Umina, P.A., Kristensen, T.N., Hoffmann, A.A. A rapidly spreading deleterious aphid endosymbiont that uses horizontal as well as vertical transmission. PNAS, 12, e2217278120 (2023)
#' 

"Rickettsiella"