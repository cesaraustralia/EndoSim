#' Show methods for S4 objects of class pest, endosym, crop, parasitoid, and endosim_mod
#' 
#' @param object S4 object of class [pest-class], [endosym-class], [crop-class], [parasitoid-class], or [endosim_mod-class]
#' 
#' @keywords methods show
#' @export
#' @docType methods
#' @aliases show,pest,ANY-method
#' @rdname show-methods

setMethod("show",
          signature(object = "pest"),
          
          function(object) {
                      cat("Pest of the species ", object@species
                      )
          }
)

#' @docType methods
#' @aliases show,endosym,ANY-method
#' @rdname show-methods

setMethod("show",
          signature(object = "endosym"),
          
          function(object) {
            cat("Endosymbiont of the species ", object@name, "\n",
                "with a fitness cost of ", object@fitness_cost, "\n",
                object@introduction_n, " infected individuals introduced on ", object@introduction_date
            )
          }
)

#' @docType methods
#' @aliases show,crop,ANY-method
#' @rdname show-methods

setMethod("show",
          signature(object = "crop"),
          
          function(object) {
            cat(object@name, " crop sown on ", object@sowing_date, "\n",
                "sown at a density of ", object@density, " plants per m2\n",
                "emerging on ", object@emergence_date, " and harvested on ", object@harvest_date
            )
          }
)

#' @docType methods
#' @aliases show,parasitoid,ANY-method
#' @rdname show-methods

setMethod("show",
          signature(object = "parasitoid"),
          
          function(object) {
            cat("Parasitoid of the species ", object@species, "\n",
                "attacking pests of the following lifestages: ", object@susc_stage, "\n",
                object@introduction_n, " individuals introduced on ", object@introduction_date
            )
          }
)

#' @docType methods
#' @aliases show,endosim_mod,ANY-method
#' @rdname show-methods

setMethod("show",
          signature(object = "endosim_mod"),
          
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