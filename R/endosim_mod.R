#' An S4 class to store a simulation output
#' 
#' @exportClass endosym_mod
#'
#' @slot pest name of the pest
#' @slot crop name of the crop
#' @slot endosymbiont name of the endosymbiont
#' @slot parasitoid name of the parasitoid
#' @slot start_date first day of simulation in YYYY-MM-DD format
#' @slot sim_length length of simulation in days
#' @slot vert_trans is vertical transmission active
#' @slot hori_trans is horizontal transmission active
#' @slot imi is immigration active
#' @slot emi is emigration active
#' @slot para is parasitoid active
#' @slot pest_df data.frame of pest population through time
#' @slot pest_cohorts array of pest cohorts at the end of the simulation
#' @slot para_df data.frame of parasitoid population through time
#' @slot area area of crop (m2)

methods::setClass("endosym_mod",
                  slots = c(pest = "character",
                            crop = "character",
                            endosymbiont = "character",
                            parasitoid = "character",
                            start_date = "character",
                            sim_length = "numeric",
                            vert_trans = "logical",
                            hori_trans = "logical",
                            imi = "logical",
                            emi = "logical",
                            para = "logical",
                            pest_df = "data.frame",
                            pest_cohorts = "array",
                            para_df = "data.frame",
                            area = "numeric")
)
