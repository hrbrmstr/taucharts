% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugins.R
\name{tau_annotations}
\alias{tau_annotations}
\title{Add a TauCharts annotations plugin}
\usage{
tau_annotations(tau, annotation_df)
}
\arguments{
\item{tau}{taucharts object}

\item{annotation_df}{a data frame with the following columns:
dim: the data dimension to annotate (i.e. name of data column)
val: the value (axis intercept) of the annotation
text: the text to be displayed at the annotation
front: if the annotation should be at in "front" of or "behind" the other data points
color: hex color for annotation}
}
\description{
Add a TauCharts annotations plugin
}
\examples{
data(cars_data)
tauchart(cars_data) \%>\%
  tau_point("milespergallon", c("class", "price"), color="class") \%>\%
  tau_annotations(data.frame(dim = "price", val = 50000,
                        text = "Whoa there!", position = "front",
                        color = '#4300FF'))
}
\seealso{
\code{\link{cars_data}} dataset
}
