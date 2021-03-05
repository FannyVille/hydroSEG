#' Display_fastest_possible_natural_hydrology
#'
#' @description
#' *Courret (2014)* shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
#' \eqn{Q= 4 * exp(-a * t^b)}
#' The value of the coefficient **a** can be expressed according to the module of the river.
#' The value of the coefficient **b** is depends on the type of variation: increase or decrease
#' The graphs show the fastest possible time in natural hydology to change from one flow to another.
#'
#' @param Stream_Module A numeric value corresponding to the average inter annual flow rate ""module"".
#' @param b_drop A numeric value of the coefficient **b** for decrease. By default b_drop = 0.75
#' @param b_rise A numeric value of the coefficient **b** for increase. By default b_rise = 1.15
#'
#' @import ggplot2
#' @importFrom  dplyr mutate
#' @importFrom  ggtext element_markdown
#' @importFrom  ggpubr ggarrange

#'
#' @return graphics
#' @export
#'
#' @examples Display_fastest_possible_natural_hydrology(10)
Display_fastest_possible_natural_hydrology <- function(Stream_Module, b_drop = 0.75, b_rise = 1.15) {
  table <- flow_variation_considered_as_fast_as_possible_in_natural_hydrology(
    Stream_Module = Stream_Module,
    b_drop = b_drop,
    b_rise = b_rise
  )
  table <- table %>%
    mutate(Percent_of_Stream_Module = c(400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5))


  Flow_increse_natural <- ggplot(data = table, mapping = aes(x = RiseTime_h, y = Percent_of_Stream_Module)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 105, alpha = 0.5, fill = "grey")+
    geom_line(color = "red") +
    geom_point(color = "red") +
    scale_y_continuous(name = "Flow ( % of Stream module)", breaks = function(x) pretty(x, n = 15), labels = function(x) paste0(x, "%")) +
    scale_x_continuous(
      name = "Time (hours)",
      breaks = function(x) pretty(x, n = 15)
    ) +
    labs(
      title = " Flow <b style='color:#FF0000'>increase </b> considered *as fast as possible* <br>
                       in natural hydrology ",
      subtitle = "Zoom from 0 to 100% of the module"
    ) +
    theme_light() +
    theme(
      plot.title = element_markdown(lineheight = 1.1, hjust = 0.05, halign = 0.5),
      plot.subtitle = element_markdown(lineheight = 1.1, hjust = 1, vjust = 0),
      axis.title.x = element_markdown(hjust = 0.27)
    )



  Flow_increse_natural_zoom <- Flow_increse_natural +
    facet_zoom2(
      ylim = c(0, 100),
      xlim = c(0, table$RiseTime_h[7]),
      horizontal = TRUE,
      zoom.size = 0.75,
      show.area = FALSE
    )



  Flow_decrease_natural <- ggplot(data = table, mapping = aes(x = DropTime_h, y = Percent_of_Stream_Module)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5, ymax = 105, alpha = 0.5, fill = "grey")+
    geom_line(color = "#006FFF") +
    geom_point(color = "#006FFF") +
    scale_y_continuous(name = "Flow ( % of Stream module)", breaks = function(x) pretty(x, n = 15), labels = function(x) paste0(x, "%")) +
    scale_x_continuous(
      name = "Time (hours)",
      breaks = function(x) pretty(x, n = 10)
    ) +
    labs(
      title = " Flow <b style='color:#006FFF'>decrease </b> considered *as fast as possible* <br>
                       in natural hydrology ",
      subtitle = "Zoom from 0 to 100% of the module"
    ) +
    theme_light() +
    theme(
      plot.title = element_markdown(lineheight = 1.1, hjust = 0.05, halign = 0.5),
      plot.subtitle = element_markdown(lineheight = 1.1, hjust = 1, vjust = 0),
      axis.title.x = element_markdown(hjust = 0.27)
    )



  Flow_decrease_natural_zoom <- Flow_decrease_natural +
    facet_zoom2(
      ylim = c(0, 100),
      xlim = c(table$DropTime_h[7], max(table$DropTime_h)),
      horizontal = TRUE,
      zoom.size = 0.75,
      show.area = FALSE
    )


  ###########





  figure <- ggarrange(Flow_decrease_natural_zoom, Flow_increse_natural_zoom, labels = c("A", "B"), ncol = 1)
  return(figure)
}
