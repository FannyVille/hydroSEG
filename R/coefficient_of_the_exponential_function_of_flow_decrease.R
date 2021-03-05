#' coefficient_of_the_exponential_function_of_flow_decrease
#'
#' @description
#' *Courret (2014)* shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
#' \eqn{Q= 4 * exp(-a * t^b)}
#' The value of the coefficient **a** can be expressed according to the module of the river.
#' This function give the **a** coefficient
#' @param Stream_Module A numeric value corresponding to the average inter annual flow rate ""module"".
#'
#' @return A number
#' @export
#'
#' @examples coefficient_of_the_exponential_function_of_flow_decrease(10)
coefficient_of_the_exponential_function_of_flow_decrease <- function(Stream_Module) {
  if (Stream_Module <1) {
    return("Outside the limits of the method" )
  }
  else if (Stream_Module <= 50) {
    return(1/(0.0000826607*Stream_Module^3-0.0104619*Stream_Module^2+0.575813*Stream_Module+2.26204))
  }
  else {
    return(1/(0.0000826607*50^3-0.0104619*50^2+0.575813*50+2.26204))
  }
}
