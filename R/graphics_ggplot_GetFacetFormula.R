GetFacetFormula <-
  function(
    facet.rows,
    facet.cols
  ){

    facet_formula <- NULL
    if(!is.null(facet.rows) | !is.null(facet.cols)){
      if(is.null(facet.rows)){
        facet.rows.formula <- "."
      } else {
        facet.rows.formula <- facet.rows
      }
      if(is.null(facet.cols)){
        facet.cols.formula <- "."
      } else {
        facet.cols.formula <- facet.cols
      }
      facet_formula <- facet_grid(paste(facet.rows.formula,"~", facet.cols.formula))
    }

    return(facet_formula)
  }
