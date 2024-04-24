#' Summary method for S4 objects of class endosim_mod or endosim_col
#'
#' @param object S4 object of class [endosim_mod-class] or [endosim_col-class]
#' @param ... Any other argument suitable for summary()
#' 
#' @keywords methods summary
#' @export
#' @docType methods
#' @aliases summary,endosim_mod,ANY-method
#' @rdname summary-methods

setMethod("summary",
          signature(object = "endosim_mod"),
          
          function(object, ...) {
            
            output <- data.frame(mean_aphid = as.numeric(object@pest_df %>%
                                                           dplyr::group_by(t) %>%
                                                           dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                           dplyr::summarise(mean = mean(`sum(...)`)/100)),
                                 aphid_peak = object@pest_df %>%
                                   dplyr::group_by(t) %>%
                                   dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                   dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                   dplyr::slice(1L) %>%
                                   dplyr::pull(`sum(...)`),
                                 peak_date = as.character(lubridate::ymd(object@start_date) +
                                                            lubridate::days(object@pest_df %>%
                                                                              dplyr::group_by(t) %>%
                                                                              dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                                              dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                                                              dplyr::slice(1L) %>% dplyr::pull(t))),
                                 sim_length = max(object@pest_df$t),
                                 end_date = as.character(lubridate::ymd(object@start_date) +
                                                           lubridate::days(max(object@pest_df$t))))
            
            output
          }
)

#' @docType methods
#' @aliases summary,endosim_col,ANY-method
#' @rdname summary-methods

setMethod("summary",
          signature(object = "endosim_col"),
          
          function(object, ...) {
            
            output <- object@scenarios
            
            output$mean_aphid <- sapply(object@sims,
                                        function(x)
                                          as.numeric(x@pest_df %>%
                                                       dplyr::group_by(t) %>%
                                                       dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                       dplyr::summarise(mean = mean(`sum(...)`)/100)))
            
            output$aphid_peak <- sapply(object@sims,
                                        function(x)
                                          x@pest_df %>%
                                          dplyr::group_by(t) %>%
                                          dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                          dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                          dplyr::slice(1L) %>%
                                          dplyr::pull(`sum(...)`))
            
            output$peak_date <- sapply(object@sims,
                                       function(x)
                                         as.character(lubridate::ymd(x@start_date) +
                                                        lubridate::days(x@pest_df %>%
                                                                          dplyr::group_by(t) %>%
                                                                          dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                                          dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                                                          dplyr::slice(1L) %>% dplyr::pull(t))))
            
            output$sim_length <- sapply(object@sims,
                                        function(x)
                                          max(x@pest_df$t))
            
            output$end_date <- sapply(object@sims,
                                      function(x)
                                        as.character(lubridate::ymd(x@start_date) +
                                                       lubridate::days(max(x@pest_df$t))))
            
            output
          }
)