#' Counterfactual Simulations
#'
#' Run multiple simulations of the endosymbiont model under different scenarios
#'
#' @param Pest object of class \code{pest} defining the pest
#' @param Endosymbiont object of class \code{endosym} defining the endosymbiont
#' @param Crop object of class \code{crop} defining the crop
#' @param Parasitoid object of class \code{parasitoid} defining the parasitoid
#' @param init object of class \code{initial} defining the starting conditions for the simulation
#' @param conds object of class \code{sim_conds} defining the simulation conditions
#' @param modules list of modules and scenarios to run, if \code{NULL} (default) all fixed to FALSE
#' @return object of class \code{endosim_col}; see [endosim_col-class].
#' @details
#' Uses the [endosim] function to run simulations of the endosymbiont model. A basic null model is always run, without any modules. The provided [pest-class], [endosym-class], [crop-class],[parasitoid-class], [initial-class], and [sim_conds-class] objects are used to parameterise the simulation.
#' 
#' \code{modules} should be a list with the following arguments: \code{vert_trans}, \code{hori_trans}, \code{imi}, \code{emi}, \code{para}. Each argument should be a logical vector, consisting either of \code{FALSE} (fixed off), \code{TRUE} (fixed on), or \code{c(FALSE, TRUE)} (run counterfactual scenarios with module turned on and turned off).
#' 
#' @seealso [endosim()]
#' @export counterfact


counterfact <- function(Pest,
                        Endosymbiont,
                        Crop,
                        Parasitoid = NULL,
                        init,
                        conds,
                        modules = NULL
) {
  if(is.null(modules))
    modules <- list(vert_trans = FALSE,
                    hori_trans = FALSE,
                    imi = FALSE,
                    emi = FALSE,
                    para = FALSE)
  
  if(any(!names(modules) %in% c("vert_trans", "hori_trans", "imi", "emi", "para")))
    stop("Unidentified module(s) provided")
  
  modules_mat <- do.call(expand.grid, modules)
  
  modules_vectors <- vector("list", nrow(modules_mat))
  for (i in 1:nrow(modules_mat)) {
    modules_vectors[[i]] <- paste(names(modules_mat), "=", modules_mat[i,], collapse = ", ")
  }
  
  print(paste0("Running ", length(modules_vectors), " counterfactual simulations"))
  
  # Default arguments for the 'endosim' function
  default_args <- list(Pest = Pest,
                       Endosymbiont = Endosymbiont,
                       Crop = Crop,
                       Parasitoid = Parasitoid,
                       init = init,
                       conds = conds,
                       plot = FALSE)
  
  output <- new("endosim_col",
                scenarios = modules_mat, 
                sims = lapply(1:length(modules_vectors),
                              function(x){
                                args_str <- modules_vectors[[x]]
                                args_list <- strsplit(args_str, ", ")[[1]]
                                parsed_args <- lapply(args_list, function(arg_str) {
                                  arg_parts <- strsplit(arg_str, "=")[[1]]
                                  argument_name <- trimws(arg_parts[1])
                                  argument_value <- eval(parse(text = trimws(arg_parts[2])))
                                  setNames(list(argument_value), argument_name)
                                })
                                
                                args <- c(default_args, do.call("c", parsed_args))
                                result <- do.call(endosim, args)
                              })
  )
  output
}