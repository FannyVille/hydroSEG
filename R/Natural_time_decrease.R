#' Natural_time_decrease
#'
#' @description
#' #' *Courret (2014)* shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
#' \eqn{Q= 4 * exp(-a * t^b)}
#' The value of the coefficient **a** can be expressed according to the module of the river.
#' The value of the coefficient **b** is depends on the type of variation: increase or decrease
#' Instead of reading the gradients graphically, this function gives directly by calculation an estimate
#' of the shortest time to go from one flow rate to another.
#'
#' @param Stream_Module A numeric value corresponding to the average inter annual flow rate ""module"".
#' @param Start A numeric value in this selection: 400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5
#' @param End A numeric value in this selection: 400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5
#'
#' @param b_drop A numeric value of the coefficient **b** for decrease. By default b_drop = 0.75
#' @param b_rise A numeric value of the coefficient **b** for increase. By default b_rise = 1.15
#'
#' @import dplyr
#' @return a text informing of the fastest duration for decrease
#' @export
#'
#' @examples Natural_time_decrease <- function(Stream_Module=10, Start=50,End=150, b_drop = 0.75, b_rise = 1.15)
Natural_time_decrease <- function(Stream_Module, Start=150,End=20, b_drop = 0.75, b_rise = 1.15) {
  table <- flow_variation_considered_as_fast_as_possible_in_natural_hydrology(
    Stream_Module = Stream_Module,
    b_drop = b_drop,
    b_rise = b_rise
  )
  table <- table %>%
    mutate(Percent_of_Stream_Module = c(400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5)) %>%
    filter(Percent_of_Stream_Module==Start|Percent_of_Stream_Module==End) %>%
    pull(DropTime_h)
  Inter<-(table[2]-table[1])
  Duration<-paste0(round(Inter,digits = 1)," hours")
  return(Duration)
}
