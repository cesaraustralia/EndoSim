#' An S4 class to store initial population sizes for the endosymbiont model
#' 
#' @exportClass initial
#'
#' @slot Pest Array describing sizes of pest cohorts of each developmental stage, divided into R+ and R- apterae and alates
#' @slot Crop Numeric vector of length 3 describing the crop. First element is proportion of R- plants (should usually be 1), second element is proportion of R+ plants (should usually be 0), third element is number of plants
#' 

methods::setClass("initial",
                  slots = c(Pest = "array",
                            Crop = "numeric")
)
