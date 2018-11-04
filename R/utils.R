## General functions for PD Progression analyses

####################################################################
## For PDF output, we want high-resolution static vector graphics
## but for HTML output, sometimes we want the interactivity provided
## by the plotly library. Pass a ggplot object to this function,
## which will decide which output to produce.
####################################################################

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
