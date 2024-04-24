#' Plot methods for S4 objects of class sim_conds, endosim_mod, endosim_col, and pest
#'
#' @param x S4 object of class [sim_conds-class] or [endosim_mod-class] or [endosim_col-class] or [pest-class]
#' @param y from the generic \code{plot} function, ignored for EndoSim objects
#' @param type "pop_size" (default) to plot population sizes through time, "R+" to plot proportion of R+ through time, "demo" to plot proportion of population by lifestage through time, "para" to plot parasitoid populations through time; ignored for class [sim_conds], [endosym_col] and [pest]
#' @param ... Any other argument suitable for \code{plot()}
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
            y1min = x@env[,2]
            y2max = x@env[,3]
            y1 = x@env[,4]
            y2 = x@env[,5]
            
            output <-
              tibble::tibble(Date = x1,
                             Temperature = y1,
                             Rainfall = y2,
                             Temp.Min = y1min,
                             Temp.Max = y2max) %>%
              tidyr::pivot_longer(cols = 2:3) %>%
              dplyr::mutate(Temp.Min = dplyr::case_when(name == "Temperature" ~ Temp.Min),
                            Temp.Max = dplyr::case_when(name == "Temperature" ~ Temp.Max)) %>%
              ggplot2::ggplot(ggplot2::aes(x = Date, y = value, colour = name)) +
              ggplot2::geom_ribbon(ggplot2::aes(ymax = Temp.Max, ymin = Temp.Min), fill = "darkred", colour = NA, alpha = .5) +
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
#' @aliases plot,endosim_mod,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "endosim_mod", y = "missing"),
          
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
                dplyr::mutate(tot = sum(n)/x@area) %>%
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
                                        dplyr::summarise(n = sum(n)/x@area)) +
                ggplot2::geom_line(data = x@pest_df %>%
                                     tidyr::pivot_longer(cols = 2:11,
                                                         names_to = "lifestage",
                                                         values_to = "n") %>%
                                     dplyr::group_by(t) %>%
                                     dplyr::summarise(n = sum(n)/x@area)) +
                ggplot2::labs(x = "Date",
                              y = "Pest density (per m2)") +
                ggplot2::scale_fill_manual(
                  values = c("darkgoldenrod", "darkgreen"),
                  name = "Phenotype",
                  labels = c("R-", "R+")
                ) +
                ggplot2::theme_bw()
            }
            
            if(type == "R+") {
              if(!x@vert_trans & !x@hori_trans)
                warning("Endosymbiont transmission modules are turned off!")
              
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
            
            if(type == "demo") {
              output <- x@pest_df %>%
                tidyr::pivot_longer(2:11) %>%
                dplyr::mutate(name = stringr::str_remove(name, "neg_|pos_")) %>%
                dplyr::group_by(t, name) %>%
                dplyr::summarise(value = sum(value)) %>%
                dplyr::ungroup() %>%
                dplyr::group_by(t) %>%
                dplyr::mutate(sumval = sum(value)) %>%
                dplyr::group_by(t, name) %>%
                dplyr::summarise(ratio = value/sumval) %>%
                dplyr::mutate(name = factor(name,
                                            levels = c("n1", "n2", "n3", "n4", "adult"),
                                            labels = c("Instar 1", "Instar 2", "Instar 3", "Instar 4", "Adult"))) %>%
                ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@start_date)), y = ratio, fill = name)) +
                ggplot2::geom_density(position = "stack", stat = "identity") +
                ggplot2::theme_minimal() +
                ggplot2::scale_fill_viridis_d(name = "Lifestage") +
                ggplot2::labs(x = "Day",
                              y = "Proportion of population")
            }
            
            if(type == "para"){
              if(!x@para)
                warning("Parasitoid module is turned off!")
              
              output <- x@para_df %>%
                tidyr::pivot_longer(cols = 2:3,
                                    names_to = "lifestage",
                                    values_to = "n") %>%
                dplyr::group_by(t, lifestage) %>%
                dplyr::summarise(n = sum(n)) %>%
                dplyr::group_by(t) %>%
                dplyr::mutate(tot = sum(n)/x@area) %>%
                ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@start_date)), y = n)) +
                ggplot2::geom_line(ggplot2::aes(colour = lifestage)) +
                ggplot2::labs(x = "Date",
                              y = "Density (per m2)") +
                ggplot2::scale_colour_manual(
                  values = c("darkred", "lightpink"),
                  name = "",
                  labels = c("Parasitoids", "Mummies")
                ) +
                ggplot2::theme_bw()
            }
            
            output
          }
)

#' @docType methods
#' @aliases plot,endosim_col,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "endosim_col", y = "missing"),
          
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
              dplyr::summarise(n = sum(n)/x@sims[[1]]@area) %>%
              ggplot2::ggplot(ggplot2::aes(x = as.Date(t, origin = as.Date(x@sims[[1]]@start_date)), y = n, colour = scenario)) +
              ggplot2::geom_line() +
              ggplot2::labs(x = "Date",
                            y = "Pest density (per m2)") +
              ggplot2::theme_bw() +
              ggplot2::theme(legend.position = "bottom",
                             legend.direction = "vertical")
            
            output
          }
)

#' @docType methods
#' @aliases plot,pest,missing,ANY-method
#' @rdname plot-methods

setMethod("plot",
          signature(x = "pest", y = "missing"),
          
          function(x, ...) {
            xvals <- seq(0, 50)
            
            output <- tibble::tibble(`Temperature-dependent development (apterae)` = x@fun_dev_apt(xvals),
                                     `Temperature-dependent development (alates)` = x@fun_dev_ala(xvals),
                                     `Temperature-dependent mortality` = sapply(xvals, function(m) x@fun_temp_loss(m, m)),
                                     `Rainfall-dependent mortality` = x@fun_rainfall_loss(xvals),
                                     `Senescence-dependent mortality` = x@fun_sen_loss(xvals),
                                     `Temperature-dependent fecundity` = x@fun_temp_fecund(xvals),
                                     `Age-dependent fecundity` = x@fun_age_fecund(xvals),
                                     `Density-dependent fecundity` = x@fun_dens_fecund(xvals * 1000),
                                     `Density-dependent alate production` = x@fun_alate_prod(xvals * 1000),
                                     xvals = xvals) %>%
              tidyr::pivot_longer(1:9) %>%
              dplyr::mutate(xvals = dplyr::case_when(stringr::str_detect(name, "Density") ~ xvals * 1000,
                                                     T ~ xvals)) %>%
              ggplot2::ggplot(ggplot2::aes(x = xvals, y = value)) +
              ggplot2::geom_line() +
              ggplot2::facet_wrap(~name, scales = "free") +
              ggplot2::theme_bw() +
              ggplot2::labs(x = "", y = "")
            
            output
          }
)