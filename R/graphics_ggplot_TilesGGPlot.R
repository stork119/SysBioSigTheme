#' TilesGGplot
#' @description TilesGGplot
TilesGGplot <-
  function(data,
           x_,
           y_,
           response_,
           point.size = 0.5,
           point.alpha = 0.1,
           point.scale = 0.25,
           colors.limits = NULL){
    x.list <-
      (data %>%
         dplyr::distinct_(x_))[[x_]]

    facet.rows.list <- NA
    facet.cols.list <- NA
    if(!is.null(facet.rows)){
      facet.rows.list <-
        (data %>%
           dplyr::distinct_(facet.rows))[[facet.rows]]
    }
    if(!is.null(facet.cols)){
      facet.cols.list <-
        (data %>%
           dplyr::distinct_(facet.cols))[[facet.cols]]
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
      data.subset <- data %>%
        dplyr::filter_(filter.string) %>%
        dplyr::arrange_(y_)
      dens <- density(x = data.subset[[y_]], n = nrow(data.subset))
      data.subset$density <- dens$y
      return(data.subset)
    } -> data.subset.list

    do.call(what = rbind,
            args = data.subset.list) ->
      data.subset.all

    if(scaled){
      scale <- 2*max(data.subset.all$density)
      data.subset.all$density.scaled <-
        data.subset.all$density/scale
    } else {
      data.subset.all %>%
        dplyr::left_join(
          (data.subset.all %>%
             dplyr::group_by_(x_) %>%
             dplyr::summarise(scale = max(density))),
          by = x_) %>%
        dplyr::mutate(density.scaled =
                        (density/scale)/scale.value
        ) ->
        data.subset.all
    }

    df.rescale <-
      (data.subset.all %>%
         dplyr::distinct_(x_) %>%
         dplyr::arrange_(x_))
    df.rescale$scaled <- 1:nrow(df.rescale)

    data.subset.all %>%
      dplyr::left_join(df.rescale) ->
      data.subset.all

    data.subset.all$position <-
      runif(min = -point.scale,
            max =  point.scale,
            n   = nrow(data.subset.all))

    g.plot <-
      ggplot(data.subset.all,
             mapping =
               aes_string(x = paste("scaled", "+", "position"),
                          y = y_,
                          group = x_,
                          position = "density",
                          color = "density")) +
      geom_point(size = point.size,
                 alpha = point.alpha) +
      scale_x_continuous(
        breaks = df.rescale$scaled,
        labels = df.rescale[[x_]]) +
      GetColorsScale(colors.limits = colors.limits) +
      SysBioSigTheme::theme_sysbiosig() +
      GetFacetFormula(facet.rows = facet.rows,
                      facet.cols = facet.cols)

    return(g.plot)
  }
