GetColorsScale <-
  function(
    colors.limits = NULL,
    position = NULL,
    ...
  ) {

    if(is.null(colors.limits)){
      scale_colors <-
        scale_color_viridis()
    } else {
      scale_colors <-
        scale_color_viridis(
          limits = colors.limits)
    }
    return(scale_colors)
  }
