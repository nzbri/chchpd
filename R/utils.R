#' Produce interactive or static graph output depending on environment
#'
#' \code{output_graph} For PDF output, we want high-resolution static vector graphics but for HTML output, sometimes we want the interactivity provided by the \code{plotly} library. Pass a \code{ggplot2} object to this function, which will decide which output to produce.
#'
#' Using \code{plotly::ggplotly()} adds interactive features to standard ggplot output (for example, tooltips can display information about individual data points). The HTML output produced by \code{ggplotly()} is, however, not valid if the destination output format for a report is either LaTeX or Word. This function checks the environment and routes a graph through \code{ggplotly()} if at the command prompt or when knitting an HTML document. If knitting to LaTeX or Word, then standard \code{ggplot2} static output will be used (e.g. PDF or PNG).
#'
#' @param graph A \code{ggplot2} graph object.
#'
#' @examples
#' \dontrun{
#' output_graph(p) # p is a ggplot object
#' }
#' @export
output_graph = function(graph){
  # find out if this document is HTML or PDF:
  outputFormat = opts_knit$get("rmarkdown.pandoc.to")

  # is NULL at command prompt when not actually knitting a document:
  if (is.null(outputFormat)) {
    plotly::ggplotly(graph) # show an interactive version
  }
  else if (outputFormat == 'html') {
    plotly::ggplotly(graph) # show an interactive version
  }
  else { # outputFormat == 'latex' or Word
    print(graph) # show a static version
  }
}

#' Clear cached data and force to download afresh from Googlesheets.
#' 
#' @examples
#' \dontrun{
#' clear_chchpd_cache()
#' }
#' @export
clear_chchpd_cache = function(){
  chchpd_env$cached = list() # reset cached data
}



output_graph2 = function(graph){
  # find out if this document is HTML or PDF:
  outputFormat = opts_knit$get("rmarkdown.pandoc.to")

  if (is.null(outputFormat))
  {# must be being done at the command prompt, i.e. not being knitted
    # to a document, so just do the standard output:
    print(graph)
  }
  # else change the graph output format appropriately:
  else if(outputFormat == 'latex') {
    # standard ggplot output of the ggplot object:
    print(graph)
  }
  else if(outputFormat == 'html') {
    # pass the ggplot object to the plotly package for
    # interactive HTML generation:
    plotly::ggplotly(graph)
  }
  else {
    # I guess this could be a Word document?
    print(graph)
  }
}
