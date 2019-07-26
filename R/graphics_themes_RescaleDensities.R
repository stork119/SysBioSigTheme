
#' rescaleSignalsValues.numeric
#'
rescaleSignalsValues.numeric <-
  function(
    numeric.scale = 1,
    ...){
    function(x){numeric.scale*x}
  }

#' rescaleSignalsValues.logarithmic
#'
rescaleSignalsValues.logarithmic <-
  function(base = exp(1),
           ...){
    function(x){log(x = x, base = base)}
  }

#' rescaleDensitiesValues
#'
#' @param density.rescale.fun parameter, that defines a function used for rescaling signals in plots.
#' There are three built-in functions, that can be chosen:
#' (1) \code{'numeric'},
#' (2) \code{logarithmic} - with base defined in \code{density.rescale.fun.args} - default: \code{e = exp(1)}.
#' Function must be defined as a lambda construct \code{function(x, ...){...}}.
#' @param density.rescale.fun.args list of the arguments to defaults \code{density.rescale.fun}
#'
rescaleDensitiesValues <-
  function(
    densities.list,
    density.rescale.fun = "numeric",
    density.rescale.fun.args = list(),
    ...
  ){
    if(!is.list(density.rescale.fun.args)){
      stop("density.rescale.fun.args must be list of arguments to density.rescale.fun")
    }
    if(is.character(density.rescale.fun)){
      rescale.option <- "numeric"
      tryCatch(
        expr = {
          rescale.option <- match.arg(arg = density.rescale.fun,
                                      choices = c("numeric",  "logarithmic", "log"))
        },
        error =
          function(e){
            warning(paste("Rescaling option density.rescale.fun =",
                          paste("'", density.rescale.fun, "'", sep = ""),
                          "is not defined. Default will be used used"))
          }
      )
      if (rescale.option %in% c("log", "logarithmic")){
        density.rescale.fun.call <-
          rescaleSignalsValues.logarithmic
      } else {
        density.rescale.fun.call <-
          rescaleSignalsValues.numeric
      }
      density.rescale.fun <-
          do.call(
            what = density.rescale.fun.call,
            args = density.rescale.fun.args)
    }

    return(density.rescale.fun(x = densities.list))
  }

