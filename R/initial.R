#' An S4 class to store initial population sizes for the endosymbiont model
#' 
#' @exportClass initial
#'
#' @slot Pest Array describing sizes of pest cohorts of each developmental stage, divided into R+ and R- apterae and alates
#' @slot Crop Dataframe where each row is an individual plant, the first column (t) describes the time of infection, the second and third columns binary representations of infection status - if R+ 1 in second column and 0 in third, if R- 0 in second column and 1 in third
#' 

methods::setClass("initial",
                  slots = c(Pest = "array",
                            Crop = "data.frame")
)
