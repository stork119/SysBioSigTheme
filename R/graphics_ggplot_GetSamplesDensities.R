GetSamplesDensities <-
  function(
    data,
    x_,
    y_,
    facet.rows,
    facet.cols,
    ...
  ){
    x.list <-
      (data %>%
         dplyr::distinct_(x_) %>%
         dplyr::arrange_(x_))[[x_]]

  facet.rows.list <- NA
  facet.cols.list <- NA
  if(!is.null(facet.rows)){
    facet.rows.list <-
      (data %>%
         dplyr::distinct_(facet.rows) %>%
         dplyr::arrange_(facet.rows))[[facet.rows]]
  }
  if(!is.null(facet.cols)){
    facet.cols.list <-
      (data %>%
         dplyr::distinct_(facet.cols) %>%
         dplyr::arrange_(facet.cols))[[facet.cols]]
  }

  expand.grid(x = x.list,
              facet.rows = facet.rows.list,
              facet.cols = facet.cols.list,
              stringsAsFactors = FALSE) ->
    groups.df


  foreach(group.i = 1:nrow(groups.df)) %do% {
    filter.string <- paste(x_, "==", groups.df[group.i,"x"])
    if(!is.null(facet.rows)){
      filter.string <-
        paste(filter.string, "&",
              facet.rows, "==", groups.df[group.i,"facet.rows"]
        )
    }
    if(!is.null(facet.cols)){
      filter.string <-
        paste(filter.string, "&",
              facet.cols, "==", groups.df[group.i,"facet.cols"]
        )
    }
    data.subset <-
      data %>%
      dplyr::filter_(filter.string) %>%
      dplyr::arrange_(y_)
    if(nrow(data.subset) != 0){
      dens <- density(x = data.subset[[y_]], n = nrow(data.subset))
      data.subset$density <- dens$y
      return(data.subset)
    } else {
      return()
    }
  } -> data.subset.list

  do.call(what = rbind,
          args = data.subset.list) ->
    data.subset.all

  rescaleDensitiesValues(
    densities.list =
      data.subset.all$density,
    ...
  ) ->
    data.subset.all$density.rescaled

  data.subset.all %>%
    dplyr::filter(!(density.rescaled %in%
                      c(NA, NaN, Inf, -Inf))) ->
    data.subset.all

  return(data.subset.all)
}
