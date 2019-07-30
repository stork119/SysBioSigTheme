GetColorsScale <-
  function(
    colors.limits = NULL,
    position = NULL,
    colors.begin = 0,
    colors.end = 1,
    ...
  ) {

    if(is.null(colors.limits)){
      scale_colors <-
        scale_color_viridis(
          begin = colors.begin,
          end = colors.end
        )
    } else {
      scale_colors <-
        scale_color_viridis(
          begin = colors.begin,
          end = colors.end,
          limits = colors.limits)
    }
    return(scale_colors)
  }
