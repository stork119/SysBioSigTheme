#' ScatterBoxplotGGplot
#' @description ScatterBoxplotGGplot
#' @param x_ - x axis
#' @param y_ - y axis
#' @param colors.limits - c(minimal color value, maximal color value)
#' @param facet.rows - column name used to create facet rows
#' @param facet.cols - column name used to create facet cols
#'
#' @inheritDotParams rescaleDensitiesValues
#' @export
ScatterBoxplotGGplot <-
  function(data,
           x_,
           y_,
           point.size = 0.5,
           point.alpha = 0.1,
           point.scale = 0.25,
           facet.rows = NULL,
           facet.cols = NULL,
           ...){

    data.subset.all <-
      GetSamplesDensities(
        data = data,
        x_ = x_,
        y_ = y_,
        facet.rows = facet.rows,
        facet.cols = facet.cols,
        ...
      )

    data.subset.all <-
      RescaleDensityData(
        data.subset.all = data.subset.all,
        ...
      )

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
                          color = "density.rescaled")) +
      geom_point(size = point.size,
                 alpha = point.alpha) +
      scale_x_continuous(
        breaks = df.rescale$scaled,
        labels = df.rescale[[x_]]) +
      GetColorsScale(...) +
      SysBioSigTheme::theme_sysbiosig(...) +
      GetFacetFormula(facet.rows = facet.rows,
                      facet.cols = facet.cols)

    return(g.plot)
  }
