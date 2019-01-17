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
#' @import AOI
#' @import leaflet
#' @importFrom crayon %+% blue yellow cyan white
#' @importFrom raster writeRaster raster mosaic extent intersect crop stack stackApply
#' @importFrom httr progress GET write_disk
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_zm read_sf as_Spatial st_as_sf st_coordinates st_write st_transform st_intersection st_buffer st_crs
#' @importFrom sp SpatialPoints SpatialPointsDataFrame CRS spTransform
#' @importFrom utils head tail write.csv unzip flush.console read.table read.delim read.csv
#' @importFrom htmlwidgets saveWidget
#' @importFrom xml2 read_xml xml_root xml_children xml_attr

NULL
