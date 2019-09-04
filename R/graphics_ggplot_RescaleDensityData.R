RescaleDensityData <-
  function(
    data.subset.all,
    scaled = TRUE,
    scale.value = 2,
    ...
    ){
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
    return(data.subset.all)
  }
