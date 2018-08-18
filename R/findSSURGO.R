#' @title Find SSURGO Maps and Mapunit Aggregated Attributes
#' @description SSURGO is a geospatial database of soils produced by the Natural Resources Conservation Service (NRCS). \code{findSSURGO}
#' returns a \code{SpatialPolygonsDataFrame} of SSURGO map units and the Mapunit aggregated attributes (muaggatt table) associated with each MUKEY. In total 43 attributes are returned,
#' all of which can be found in the [SSURGO documentation](https://sdmdataaccess.sc.egov.usda.gov/documents/TableColumnDescriptionsReport.pdf). The MUAGGATT values cover a wide range of
#' soil realted questions however if a user is interested in the full range of tabular data associated with the SSURGO map units we suggest checking out the FedData package.
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @return a list() of minimum length 2: AOI and ssurgo
#' @examples
#' \dontrun{
#' AOI  = getAOI(clip = list("UCSB", 10, 10)) %>% findSSURGO()
#' }
#' @author Mike Johnson
#' @export
#'

findSSURGO <- function(AOI, table = FALSE) {

  `%+%` = crayon::`%+%`

  if (!(class(AOI) %in% c("list", "HydroData"))) {AOI = list(AOI = AOI)}

  bb.st = AOI$AOI %>% AOI::bbox_st()
  AOI.sf = sf::st_as_sf(AOI$AOI)
  AOI.sf = sf::st_transform(AOI.sf, "+proj=longlat +datum=WGS84")

  if (any((bb.st$xmax - bb.st$xmin) > 1, (bb.st$ymax - bb.st$ymin) > 1)) {
    grid = sf::as_Spatial(sf::st_make_grid(AOI.sf, cellsize = c(.95, .95)))
  } else {
    grid = AOI.sf
  }

  input.shp = list()

  cat(crayon::cyan("Locating regions\n"))

  for (i in seq_along(grid)) {
    bound <- grid[i] %>% AOI::bbox_st()
    url <-
      paste0(
        "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&Typename=SurveyAreaPoly&BBOX=",
        paste(bound$xmin,
              bound$ymin,
              bound$xmax,
              bound$ymax,
              sep = ",")
      )

    check = download.url(url)
    sf = sf::read_sf(check$destfile)
    sf::st_crs(sf) = "+proj=longlat +datum=WGS84"
    sf$saverest <- as.Date(sf$saverest, format = "%b %d %Y")
    input.shp[[i]] =  suppressWarnings(suppressMessages(sf::st_intersection(sf, AOI.sf)))
  }

  regions = do.call(rbind, input.shp)

  input.shp = list()

  for (i in 1:NROW(regions)) {
    url <- paste0("http://websoilsurvey.sc.egov.usda.gov/DSD/Download/Cache/SSA/wss_SSA_",
                  as.character(regions$areasymbol[i]),
                  "_[",
                  as.Date(regions$saverest[i], format = "%m/%d/%Y"),
                  "].zip" )

    cat(crayon::white(paste0( "Downloading (", i, "/", NROW(regions), "): ")) %+% crayon::yellow(basename(url), "\n"))

    check = download.url(url)
    unzip(check$destfile, exdir = tempdir(), overwrite = TRUE)
    shp.path =  list.files( tempdir(),
                       full.names = T,
                       recursive = T,
                       pattern = paste0("soilmu_a_", tolower(as.character(regions$areasymbol[i])), ".shp")
                     )
    sf =  sf::read_sf(shp.path)
    input.shp[[i]] = suppressWarnings(suppressMessages(sf::st_intersection(sf, AOI.sf)))
  }

  if(length(input.shp) > 1){ soils = do.call(rbind, input.shp) }else{ soils = input.shp[[1]] }

  tab = do.call(rbind,
               lapply(list.files(tempdir(), recursive = T, pattern = 'muaggatt', full.names = T),
                      function(e){ read.delim(e, header = F, sep = "|", stringsAsFactors = F) }
               ))

  names(tab) = names(read.csv( paste0("https://casoilresource.lawr.ucdavis.edu/soil_web/component_data.php?mukey=95439&action=muaggatt&format=csv")))[-1]

  soils = merge(soils, tab, by.x = "MUKEY", by.y = "mukey")

  AOI[["ssurgo"]] = sf::as_Spatial(soils)

  cat( crayon::white("Returned object contains: ") %+% crayon::green("cropped SSURGO Soils Map\n") )

  return(AOI)
}



