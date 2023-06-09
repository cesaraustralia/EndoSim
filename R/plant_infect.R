#' Plant infections
#'
#' Calculate number of infected plants per time step
#'
#' @param Ph Population of healthy plants (Plants/m2)
#' @param Pi Population of infected plants (Plants/m2)
#' @param Awt Density of wild type insects (Insects/m2)
#' @param Ai Density of infected insects (Insects/m2)
#' @param Awtp Number of newly produced wild type insects in time step (Insects/m2)
#' @param Aip Number of newly produced infected insects in time step (Insects/m2)
#' @param Ac Carrying capacity of paddock (Insects/m2)
#' @param temperature Mean temperature during time step (C)
#' @param prop_day Proportion of day within temperature thresholds
#' @param Amt Movements of insects per day
#' @return \code{Pi} the new population of infected plants at time step
#' @details
#' Calculate the new population of infected plants at the end of the time step, using the following set of equations:
#' \deqn{
#' A_{f} = \frac{{A_{wt} + A_{i}}}{{A_{c}}}
#' }
#'
#' \deqn{
#' P_{n} = \frac{{A_{mt} \cdot A_{i}}}{{(A_{wt} + A_{i}) \cdot A_{n}}}
#' }
#'
#' \deqn{
#' P_{i} = P_{h} \cdot \left(1 - e^{-\frac{{P_{n}}}{{P_{h} + P_{i}}}}\right)
#' }
#' @keywords internal

plant_infect <-
  function(Ph,
           Pi,
           Awt,
           Ai,
           Awtp,
           Aip,
           Ac,
           temperature,
           prop_day,
           Amt) {
    # a. calculate current fraction of carrying capacity
    Af = (Awt + Ai) / Ac
    # b. calculate transmission efficiency
    An = f_inoc(temperature) * prop_day
    # c. calculate successful plant inoculations
    Pn = (Amt * Ai) / ((Awt + Ai) * An)
    # d. calculate new number of infected plants
    Pi = Ph * (1 - exp(-Pn / (Ph + Pi)))
    
    return(max(0, Pi))
  }