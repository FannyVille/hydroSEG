#' flow_variation_considered_as_fast_as_possible_in_natural_hydrology
#'
#'@description
#' *Courret (2014)* shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
#' \eqn{Q= 4 * exp(-a * t^b)}
#' The value of the coefficient **a** can be expressed according to the module of the river.
#' The value of the coefficient **b** is depends on the type of variation: increase or decrease
#'
#' @param Stream_Module A numeric value corresponding to the average inter annual flow rate ""module"".
#' @param b_drop A numeric value of the coefficient **b** for decrease. By default b_drop = 0.75
#' @param b_rise A numeric value of the coefficient **b** for increase. By default b_rise = 1.15
#'
#' @importFrom tibble tibble
#' @import magrittr
#' @importFrom janitor adorn_pct_formatting
#' @return A table which reconstitutes the fastest variations in natural regime according to the module
#' @export
#'
#' @examples flow_variation_considered_as_fast_as_possible_in_natural_hydrology(10)
flow_variation_considered_as_fast_as_possible_in_natural_hydrology <- function(Stream_Module, b_drop = 0.75, b_rise = 1.15) {
  table <- tibble(
    Percent_of_Stream_Module = c(
      4, 3.5, 3, 2.5, 2, 1.5, 1, 0.75,
      0.50, 0.25, 0.15, 0.10, 0.05,
      0.04, 0.03, 0.02, 0.01, 0.005
    ),
    Flow_cms = Percent_of_Stream_Module * Stream_Module,
    DropTime_h = ((log(4) - log(Percent_of_Stream_Module)) /
                    coefficient_of_the_exponential_function_of_flow_decrease(Stream_Module))
    ^(1 / b_drop),
    RiseTime_computation = ((log(4) - log(Percent_of_Stream_Module)) /
                              coefficient_of_the_exponential_function_of_flow_increase(Stream_Module))
    ^(1 / b_rise),
    RiseTime_h = max(RiseTime_computation) - RiseTime_computation
  )
  return(table %>% adorn_pct_formatting(, , , Percent_of_Stream_Module))
}
