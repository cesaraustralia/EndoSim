#' An S4 class to store a pest's life history for the endosymbiont model
#' 
#' @exportClass pest
#'
#' @slot species The name of the pest
#' @slot bg_loss Background mortality (0-1)
#' @slot alate_penalty Relative fecundity penalty of alates compaered to apterae (0-1)
#' @slot apterae_walk Number of plants per day visited by apterae
#' @slot alate_flight Number of plants per day visited by alates
#' @slot fun_dev_apt Function describing temperature-dependent development of nymphs destined to become apterae
#' @slot fun_dev_ala Function describing temperature-dependent development of nymphs destined to become alates
#' @slot fun_imi_neg Function describing number of immigrant R- alates
#' @slot fun_imi_pos Function describing number of immigrant R+ alates
#' @slot fun_emi Function describing number of emigrants among newly metamorphosed alates
#' @slot fun_temp_loss Function describing daily mortality due to exceeding temperature thresholds
#' @slot fun_rainfall_loss Function describing daily mortality due to rainfall
#' @slot fun_sen_loss Function describing daily mortality due to senescence
#' @slot fun_dens_fecund Function describing daily fecundity as function of pest density
#' @slot fun_temp_fecund Function describing daily fecundity as function of temperature
#' @slot fun_age_fecund Function describing daily fecundity as function of adult cohort age
#' @slot fun_alate_prod Function describing proportion of newly produced nymphs destined to become alates as function of pest density
#' 
#' @examples 
#' GPA <-
#' new("pest",
#'   species = "Myzus persicae",
#'   bg_loss = 0.03,
#'   alate_penalty = 0.5,
#'   apterae_walk = 0.0005,
#'   alate_flight = 0.005,
#'   fun_dev_apt = EndoSim:::fit_wang(24.1, 0.152, 18.09, 32.79, 2.84, 1.8977),
#'   fun_dev_ala = EndoSim:::fit_wang(24.1, 0.16474, 19.96, 33.17, 1.41, 2.2202),
#'   fun_imi_neg = EndoSim:::fit_imi_barton(),
#'   fun_imi_pos = EndoSim:::fit_null(0),
#'   fun_emi = EndoSim:::fit_emi_thack(),
#'   fun_temp_loss = EndoSim:::fit_supergaus(32.79, 2.84, 0.00083, 0.00017, 7.66, 17.66),
#'   fun_rainfall_loss = EndoSim:::fit_rainfall_thack(),
#'   fun_sen_loss = EndoSim:::fit_gompertz(0.00188, 0.13),
#'   fun_dens_fecund = EndoSim:::fit_bannerman(10000, 0.0008),
#'   fun_temp_fecund = EndoSim:::fit_quadratic(-0.7611, 31.9847, 25),
#'   fun_age_fecund = EndoSim:::fit_weibull(58, 1.885, 5.953, 0),
#'   fun_alate_prod = EndoSim:::fit_alate(67.418, 0.993, 0.076, 300)
#'  )
#' 
#' GPA

methods::setClass("pest",
                  slots = c(species = "character",
                            bg_loss = "numeric",
                            alate_penalty = "numeric",
                            apterae_walk = "numeric",
                            alate_flight = "numeric",
                            fun_dev_apt = "function",
                            fun_dev_ala = "function",
                            fun_imi_neg = "function",
                            fun_imi_pos = "function",
                            fun_emi = "function",
                            fun_temp_loss = "function",
                            fun_rainfall_loss = "function",
                            fun_sen_loss = "function",
                            fun_dens_fecund = "function",
                            fun_temp_fecund = "function",
                            fun_age_fecund = "function",
                            fun_alate_prod = "function")
)
