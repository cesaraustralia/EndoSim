#' Show method for S4 object of class endosim_mod
#' 
#' @param object object of class [endosim_mod-class]
#' 
#' @keywords methods show
#' @export
#' @docType methods
#' @rdname show-methods

setGeneric("show", function(object) standardGeneric("show"))

#' @docType methods
#' @aliases show,endosim_mod,ANY-method
#' @rdname show-methods

setMethod("show",
          "endosim_mod",
          
          function(object) {
            if(object@vert_trans)
              vert_trans <- "Vertical transmission" else
                vert_trans <- NULL
              
              if(object@hori_trans)
                hori_trans <- "Horizontal transmission" else
                  hori_trans <- NULL
                
                if(object@imi)
                  imi <- "Immigration" else
                    imi <- NULL
                  
                  if(object@emi)
                    emi <- "Emigration" else
                      emi <- NULL
                    
                    if(object@para)
                      para <- "Parasitoid" else
                        para <- NULL
                    
            cat("Endosymbiont model simulation\n",
                "Pest: ", object@pest, "\n",
                "Crop: ", object@crop, "\n",
                "Endosymbiont: ", object@endosymbiont, "\n",
                "Parasitoid: ", object@parasitoid, "\n",
                "Started on ", object@start_date, ", running for ", object@sim_length, " days\n",
                "With the following modules: ", paste(c(vert_trans, hori_trans, imi, emi, para), collapse = ", ")
                )
          }
)