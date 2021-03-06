% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{output_graph}
\alias{output_graph}
\title{Produce interactive or static graph output depending on environment}
\usage{
output_graph(graph)
}
\arguments{
\item{graph}{A \code{ggplot2} graph object.}
}
\description{
\code{output_graph} For PDF output, we want high-resolution static vector graphics but for HTML output, sometimes we want the interactivity provided by the \code{plotly} library. Pass a \code{ggplot2} object to this function, which will decide which output to produce.
}
\details{
Using \code{plotly::ggplotly()} adds interactive features to standard ggplot output (for example, tooltips can display information about individual data points). The HTML output produced by \code{ggplotly()} is, however, not valid if the destination output format for a report is either LaTeX or Word. This function checks the environment and routes a graph through \code{ggplotly()} if at the command prompt or when knitting an HTML document. If knitting to LaTeX or Word, then standard \code{ggplot2} static output will be used (e.g. PDF or PNG).
}
\examples{
\dontrun{
output_graph(p) # p is a ggplot object
}
}
