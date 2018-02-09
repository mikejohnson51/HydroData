#' Clip and merge raster data
#'
#' Clip raster data to a set boundary and merge
#'
#' @param dataset a list of rasters
#' @param bounds a boundary to clip to
#' @import raster
#' @export
#' @author
#' Mike Johnson


clipMerge = function(dataset, bounds){

  for(j in 1:length(dataset)){
    if(!is.null(intersect(extent(dataset[[j]]), extent(bounds)))){
      dataset[[j]] <- crop(dataset[[j]], bounds)
      message("Raster number ", j, " of ", length(dataset), " Cropped.")
    } else {
      message("Raster ", j, " not needed")
    }
  }

  if(length(dataset) > 1){
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
