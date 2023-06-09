#' Check species object
#'
#' Checks the validity of a species object
#'
#' @param object An object of class \code{species}; see [species]
#' 

check_species <- function(object){
  errors <- character()
  
  val_reproduction <- object@reproduction
  if (!val_reproduction %in% c("sexual", "asexual")){
    msg <- paste("reproduction is ", val_reproduction, ".  Should be either `sexual` or `asexual`", sep = "")
    errors <- c(errors, msg)
  }
  
  val_development <- object@development
  if (!val_development %in% c("holometabolous", "hemimetabolous")){
    msg <- paste("development is ", val_development, ".  Should be either `holometabolous` or `hemimetabolous`", sep = "")
    errors <- c(errors, msg)
  }
  
  length_growth <- length(formals(object@f_growth))
  if (length_growth != 1) {
    msg <- paste("f_growth accepts ", length_age, " arguments.  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  length_loss <- length(formals(object@f_loss))
  if (length_loss != 1) {
    msg <- paste("f_growthloss accepts ", length_age, " arguments.  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  length_suscept <- length(formals(object@f_suscept))
  if (length_suscept != 1) {
    msg <- paste("f_suscept accepts ", length_age, " arguments.  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  length_inoc <- length(formals(object@f_inoc))
  if (length_inoc != 1) {
    msg <- paste("f_inoc accepts ", length_age, " arguments.  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  length_alate <- length(formals(object@f_alate))
  if (length_alate != 1) {
    msg <- paste("f_alate accepts ", length_age, " arguments.  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}