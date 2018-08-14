#' HydroData
#'
#' \code{HydroData} package
#'
#' Access earth systems geospatial and observation data programatically
#'
#' See the README on
#'
#' @docType package
#' @name HydroData
#'
#' @importFrom raster writeRaster raster mosaic extent intersect crop
#' @importFrom httr progress GET write_disk
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_zm read_sf as_Spatial st_as_sf st_coordinates st_write st_transform st_intersection
#' @importFrom sp SpatialPoints SpatialPointsDataFrame CRS spTransform
#' @importFrom utils tail write.csv download.file unzip flush.console

NULL
