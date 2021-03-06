% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Natural_time_increase.R
\name{Natural_time_increase}
\alias{Natural_time_increase}
\title{Natural_time_increase}
\usage{
Natural_time_increase(
  Stream_Module,
  Start = 50,
  End = 150,
  b_drop = 0.75,
  b_rise = 1.15
)
}
\arguments{
\item{Stream_Module}{A numeric value corresponding to the average inter annual flow rate ""module"".}

\item{Start}{A numeric value in this selection: 400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5}

\item{End}{A numeric value in this selection: 400, 350, 300, 250, 200, 150, 100, 75, 50, 25, 15, 10, 5, 4, 3, 2, 1, 0.5}

\item{b_drop}{A numeric value of the coefficient \strong{b} for decrease. By default b_drop = 0.75}

\item{b_rise}{A numeric value of the coefficient \strong{b} for increase. By default b_rise = 1.15}
}
\value{
a text informing of the fastest duration for increase
}
\description{
#' \emph{Courret (2014)} shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
\eqn{Q= 4 * exp(-a * t^b)}
The value of the coefficient \strong{a} can be expressed according to the module of the river.
The value of the coefficient \strong{b} is depends on the type of variation: increase or decrease
Instead of reading the gradients graphically, this function gives directly by calculation an estimate
of the shortest time to go from one flow rate to another.
}
\examples{
Natural_time_increase <- function(Stream_Module=10, Start=50,End=150, b_drop = 0.75, b_rise = 1.15)
}
