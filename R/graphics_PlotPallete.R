#' PlotPallete
#' @description PlotPallete
#' @param colors.pallete -- vector of strings with colors names
#' @param dim.x -- number of tiles on x axes
#' @param dim.y -- number of tiles on y axes
#' @param text.color -- color of color name label on tile
#' @export
PlotPallete <-
  function(colors.pallete = c(),
           dim.x = 4,
           dim.y = NULL,
           text.color = "white",
           ...){
    pallete.size <- length(colors.pallete)
    if(dim.x > pallete.size){
      dim.y <- 1
    } else {
      dim.y <- ceiling(pallete.size/dim.x)
    }
    expand.grid(
      x = 1:dim.x,
      y = 1:dim.y,
      stringsAsFactors = FALSE) %>%
      dplyr::arrange(
        x,y
      ) ->
      df
    df <- df[1:pallete.size,]
    df$colors <- colors.pallete
    names(colors.pallete) <- colors.pallete
    ggplot(
      data = df,
      mapping =
        aes(
          x = x,
          y = y,
          fill = colors,
          label  = colors
        )
    ) +
      geom_tile() +
      geom_text(color = text.color) +
      scale_fill_manual(
        values = colors.pallete,
        name = "pallete") +
      theme_sysbiosig()
  }
