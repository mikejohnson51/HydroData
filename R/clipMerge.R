#' Clip and Merge raster data
#'
#' @details
#'  \code{clipMerge} clip raster brick(s) to a set boundary and merge
#'
#' @param dataset a raster stack or brick
#' @param bounds a clipping boundary
#'
#' @import raster
#'
#' @family HydroData 'helper' function
#' @export
#' @author
#' Mike Johnson


clipMerge = function(dataset, bounds) {

  # Check if raster i is within bounds...
  for (j in 1:length(dataset)) {
    if (!is.null(intersect(extent(dataset[[j]]), extent(bounds)))) {
      dataset[[j]] <- crop(dataset[[j]], bounds)
      message("Raster number ", j, " of ", length(dataset), " Cropped.")
    } else {
      message("Raster ", j, " not needed")
    }
  }

  # If dataset is greater then one, mosaic pieces...

  if (length(dataset) > 1) {
    message("Mosaicing raster for ", dataset[j])
    utils::flush.console()
    dataset$fun <- max
    dataset$na.rm <- TRUE
    mos = do.call(mosaic, dataset)
    gc()
  } else {
    mos = dataset[[1]]
  }
  return(mos)
}
