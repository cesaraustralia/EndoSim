#' Summary method for S4 object of class endosim_col
#'
#' @param x S4 object of class \code{endosim_col}
#' @param ... Any other argument suitable for summary()
#' 
#' @keywords methods summary
#' @export
#' @docType methods
#' @rdname summary-methods

setGeneric("summary", function(x, ...) standardGeneric("summary"))

#' @docType methods
#' @aliases summary,endosim_col,missing,ANY-method
#' @rdname summary-methods

setMethod("summary",
          signature(x = "endosim_col"),
          
          function(x, ...) {
            
            output <- x@scenarios
            
            output$mean_aphid <- sapply(x@sims,
                                        function(x)
                                          as.numeric(x@pest_df %>%
                                                       dplyr::group_by(t) %>%
                                                       dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                       dplyr::summarise(mean = mean(`sum(...)`)/100)))
            
            output$aphid_peak <- sapply(x@sims,
                                        function(x)
                                          x@pest_df %>%
                                          dplyr::group_by(t) %>%
                                          dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                          dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                          dplyr::slice(1L) %>%
                                          dplyr::pull(`sum(...)`))
            
            output$peak_date <- sapply(x@sims,
                                       function(x)
                                         as.character(lubridate::ymd(x@start_date) +
                                                        lubridate::days(x@pest_df %>%
                                                                          dplyr::group_by(t) %>%
                                                                          dplyr::summarise(sum(pos_n1, neg_n1, pos_n2, neg_n2, pos_n3, neg_n3, pos_n4, neg_n4, pos_adult, neg_adult)) %>%
                                                                          dplyr::arrange(dplyr::desc(`sum(...)`)) %>%
                                                                          dplyr::slice(1L) %>% dplyr::pull(t))))
            
            output$sim_length <- sapply(x@sims,
                                        function(x)
                                          max(x@pest_df$t))
            
            output$end_date <- sapply(x@sims,
                                      function(x)
                                        as.character(lubridate::ymd(x@start_date) +
                                                       lubridate::days(max(x@pest_df$t))))
            
            output
          }
)