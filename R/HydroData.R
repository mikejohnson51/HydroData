#' \code{HydroData} package
#'
#' Access climate, landscape and hydrolgic data programatically
#'
#' See the README on
#' #href{https://cran.r-project.org/package=googlesheets/README.html}{CRAN}
#' #href{https://github.com/jennybc/googlesheets#readme}{GitHub}
#'
#' @docType package
#' @name HydroData
#'
#' @importFrom  tidyr drop_na gather
#'
#' @importFrom  dplyr filter mutate %>% arrange
#'
#' @importFrom  sp SpatialPointsDataFrame spTransform SpatialPoints CRS SpatialPolygons spDistsN1 Polygon Polygons
#'
#' @importFrom  utils download.file unzip data type.convert
#'
#' @importFrom  methods as
#'
#' @importFrom  stats setNames
#'
#' @importFrom  dismo geocode
#'
#' @importFrom  RCurl getURL
#'
#' @importFrom  data.table fread
#'
#' @importFrom  XML xmlToDataFrame
#'
#' @importFrom  ncdf4 nc_open nc_close ncvar_get ncatt_get
#'
#' @importFrom  lubridate ymd_hms
#'
#' @importFrom  leaflet leaflet colorQuantile addTiles addPolygons addLegend
#'
#' @importFrom  rgeos gIntersection
#'
#' @importFrom  USAboundaries us_counties us_states

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  { utils::globalVariables(c("LAT", "LON",
                                                        "State", "Longitude", "Latitude",
                                                        "nid_cleaned",
                                                        "usgsStations",
                                                        "snotel",
                                                        "daymet_tiles",
                                                        "kopRas",
                                                        "DAY", "site_no", "YEAR", "MONTH",
                                                        "year_2000", "year_2005", "year_2010", "COUNTY", "ID",
                                                        "PARAMETER", "Date", "."))
}

