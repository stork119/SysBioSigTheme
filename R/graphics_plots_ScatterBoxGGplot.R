#' ScatterBoxplotGGplot
#' @description ScatterBoxplotGGplot
#' @export
ScatterBoxplotGGplot <-
  function(data,
           x_,
           y_,
           point.size = 0.5,
           point.alpha = 0.5,
           point.scale = 0.25,
           scaled = TRUE){
    x.list <-
      (data %>%
         dplyr::distinct_(x_))[[x_]]

    foreach(x = x.list) %do% {
      data.subset <- data %>%
        dplyr::filter_(paste(x_, "==", x)) %>%
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
                        (density/scale)/2
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
      scale_color_viridis() +
      SysBioSigTheme::theme_sysbiosig()
  }
