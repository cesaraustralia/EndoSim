#' Plot methods for S4 objects of class sim_conds, endosym_mod, and endosym_col
#'
#' @param x S4 object of class \code{sim_conds} or \code{endosym_mod} or \code{endosym_col}
#' @param y from the generic \code{plot} function, ignored for EndoSim objects
#' @param type "pop_size" (default) to plot population sizes through time, "R+" to plot proportion of R+ through time; ignored for class \code{sim_conds}
#' @param ... Any other argument suitable for plot()
#' 
#' @keywords methods plot
#' @export
#' @docType methods
#' @rdname plot-methods

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

#' @docType methods
#' @aliases plot,sim_conds,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "sim_conds", y = "missing"),
          
          function(x, ...) {
            x1 = lubridate::ymd(x@start_date) + lubridate::days(x@env[,1] - 1)
            y1 = x@env[,2]
            y2 = x@env[,3]
            
            output <-
              tibble::tibble(Date = x1,
                             Temperature = y1,
                             Rainfall = y2) %>%
              tidyr::pivot_longer(cols = 2:3) %>%
              ggplot2::ggplot(ggplot2::aes(x = Date, y = value, colour = name)) +
              ggplot2::geom_line() +
              ggplot2::scale_colour_manual(values = c("darkblue", "darkred")) +
              ggplot2::theme_bw() +
              ggplot2::labs(y = "") +
              ggplot2::facet_wrap(~name, ncol = 1, scales = "free") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                             legend.position = "none")
            
            output
          }
)

#' @docType methods
#' @aliases plot,endosym_mod,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "endosym_mod", y = "missing"),
          
          function(x, type = "pop_size", ...) {
            if(type == "pop_size") {
              output <- x@pest_df %>%
                tidyr::pivot_longer(cols = 2:11,
                                    names_to = "lifestage",
                                    values_to = "n") %>%
                dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                              lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
                dplyr::group_by(t, endosymbiont) %>%
                dplyr::summarise(n = sum(n)) %>%
                dplyr::group_by(t) %>%
                dplyr::mutate(tot = sum(n)) %>%
                ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@start_date)), y = n)) +
                ggplot2::geom_density(ggplot2::aes(fill = endosymbiont),
                                      stat = "identity",
                                      colour = NA,
                                      position = "stack",
                                      data = x@pest_df %>%
                                        tidyr::pivot_longer(cols = 2:11,
                                                            names_to = "lifestage",
                                                            values_to = "n") %>%
                                        dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                                                      lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
                                        dplyr::group_by(t, endosymbiont) %>%
                                        dplyr::summarise(n = sum(n))) +
                ggplot2::geom_line(data = x@pest_df %>%
                                     tidyr::pivot_longer(cols = 2:11,
                                                         names_to = "lifestage",
                                                         values_to = "n") %>%
                                     dplyr::group_by(t) %>%
                                     dplyr::summarise(n = sum(n))) +
                ggplot2::labs(x = "Date",
                              y = "Total number of pests") +
                ggplot2::scale_fill_manual(
                  values = c("darkgoldenrod", "darkgreen"),
                  name = "Phenotype",
                  labels = c("R-", "R+")
                ) +
                ggplot2::theme_bw()
            }
            
            if(type == "R+"){
              output <- x@pest_df %>%
                tidyr::pivot_longer(cols = 2:11,
                                    names_to = "lifestage",
                                    values_to = "n") %>%
                dplyr::mutate(endosymbiont = ifelse(stringr::str_detect(lifestage, "pos"), "pos", "neg"),
                              lifestage = sub("_", "", stringr::str_remove(lifestage, "pos|neg|"))) %>%
                dplyr::group_by(t, endosymbiont) %>%
                dplyr::summarise(n = sum(n)) %>%
                dplyr::group_by(t) %>%
                dplyr::summarise(tot = sum(n),
                                 prop = n / tot) %>%
                dplyr::slice(2L) %>%
                ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@start_date)), y = prop)) +
                ggplot2::geom_line(colour = "darkred", size = 1) +
                ggplot2::lims(y = c(0, 1)) +
                ggplot2::labs(x = "Date",
                              y = "Proportion of R+ in population") +
                ggplot2::theme_bw()
            }
            
            output
          }
)

#' @docType methods
#' @aliases plot,endosym_col,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "endosym_col", y = "missing"),
          
          function(x, ...) {
            all_scenarios <- lapply(1:dim(x@scenarios)[1],
                                    function(i) x@sims[[i]]@pest_df %>%
                                      dplyr::mutate(vert_trans = x@scenarios[i, 1],
                                                    hori_trans = x@scenarios[i, 2],
                                                    imi = x@scenarios[i, 3],
                                                    emi = x@scenarios[i, 4],
                                                    para = x@scenarios[i, 5],
                                                    scenario = paste0("vert_trans = ", model_all@scenarios[i, 1],
                                                                      ", hori_trans = ", model_all@scenarios[i, 2],
                                                                      ", imi = ", model_all@scenarios[i, 3],
                                                                      ", emi = ", model_all@scenarios[i, 4],
                                                                      ", para = ", model_all@scenarios[i, 5])))
            
            output <- do.call(rbind, all_scenarios) %>%
              tidyr::pivot_longer(cols = 2:11,
                                  names_to = "lifestage",
                                  values_to = "n") %>%
              dplyr::group_by(t, vert_trans, hori_trans, imi, emi, para, scenario) %>%
              dplyr::summarise(n = sum(n)) %>%
              ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@sims[[1]]@start_date)), y = n, colour = scenario)) +
              ggplot2::geom_line() +
              ggplot2::labs(x = "Date",
                            y = "Total number of pests") +
              ggplot2::theme_bw() +
              ggplot2::theme(legend.position = "bottom",
                             legend.direction = "vertical")
            
            output
          }
)