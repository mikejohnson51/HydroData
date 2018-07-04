#' Get Bounding box
#'
#' @description A function to define a minimum bounding box for a set of points
#'
#' @param x a \code{data.frame} with a lat and long column
#'
#' @return a \code{SpatialPolygon} bounding area of input points \code{x}
#'
#' @family HydroData 'utility' function

getBoundingBox = function(x) {
  coords = matrix(
    c(
      min(x$long),
      min(x$lat),
      min(x$long),
      max(x$lat),
      max(x$long),
      max(x$lat),
      max(x$long),
      min(x$lat),
      min(x$long),
      min(x$lat)
    ),
    ncol = 2,
    byrow = TRUE
  )

  bb = Polygon(coords)
  bb = SpatialPolygons(list(Polygons(list(bb), ID = "AOI")), proj4string = CRS("+init=epsg:4326"))

  return(bb)
}

