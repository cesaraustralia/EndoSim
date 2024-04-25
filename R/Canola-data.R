#' Canola crop information
#'
#' A crop object defining a canola crop for simulations
#' during the 2022 growing season.\cr
#' \cr
#' \code{heal_time} is set at 2 days \cr
#' \code{sowing_date} is 2022-04-05 (early sowing date) \cr
#' \code{emergence_date} is 2022-04-10 \cr
#' \code{harvest_date} is 2022-11-02 \cr
#' \code{carrying_capacity} fitted using [fit_sigmoid] so that crop reaches maximum carrying capacity within 70 days post emergence \cr
#' \code{density} is set at 35 plants/m^2^ \cr
#'
#' @docType data
#'
#' @usage data(Canola)
#'
#' @format An object of class \code{crop}; see [crop-class].
#'
#' @keywords datasets
#'

"Canola"