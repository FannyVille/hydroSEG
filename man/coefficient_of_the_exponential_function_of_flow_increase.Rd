% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/coefficient_of_the_exponential_function_of_flow_increase.R
\name{coefficient_of_the_exponential_function_of_flow_increase}
\alias{coefficient_of_the_exponential_function_of_flow_increase}
\title{coefficient_of_the_exponential_function_of_flow_increase}
\usage{
coefficient_of_the_exponential_function_of_flow_increase(Stream_Module)
}
\arguments{
\item{Stream_Module}{A numeric value corresponding to the average inter annual flow rate ""module"".}
}
\value{
A number
}
\description{
\emph{Courret (2014)} shows that in natural hydrology the rise and fall times of hydrographs follow an exponential law.
\eqn{Q= 4 * exp(-a * t^b)}
The value of the coefficient \strong{a} can be expressed according to the module of the river.
This function give the \strong{a} coefficient
}
\examples{
coefficient_of_the_exponential_function_of_flow_increase(10)
}
