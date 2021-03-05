#' Converts a flow value to a percentage of the module
#'
#' @param Stream_Module A numeric value corresponding to the average interannual flow rate ""module"".
#' @param Flow A numerical value corresponds to a flow rate which we wish to convert into a percentage of the average interannual flow rate "module".
#'
#' @return Numerical value of the "double" type which corresponds to the percentage flow rate of the module. Exemple: if Stream_Module=10 and Flow=16, then the flow rate in percentage of the module is 160%
#' @export
#' @examples Convertion_to_mod_percentage(Stream_Module=10,Flow=16)
Convertion_to_mod_percentage <- function(Stream_Module,Flow) {
  return(Flow*100/Stream_Module)
}
