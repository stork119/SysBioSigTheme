#' normalize_data
#'
#' @export
normalize_data <- function(data,
                           normalize_factor = 65535){

  data.intensity_colnames <- grepl("Intensity", colnames(data)) &
    !grepl("Location", colnames(data))
  data[,data.intensity_colnames] <- data[,data.intensity_colnames]*normalize_factor
  return(list(data = data))
}
