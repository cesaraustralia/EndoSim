#' An S4 class to store initial population sizes for the endosymbiont model
#' 
#' @exportClass initial
#'
#' @slot Pest Array describing sizes of pest cohorts of each developmental stage, divided into R+ and R- apterae and alates
#' @slot Crop Numeric vector of length 2 describing the crop. First element is proportion of R- plants (should usually be 1), second element is proportion of R+ pplants (should usually be 0)
#' @slot Parasitoid Numeric describing how many adult female parasitoids at start of simulation
#' 

methods::setClass("initial",
                  slots = c(Pest = "array",
                            Crop = "numeric",
                            Parasitoid = "numeric")
)
