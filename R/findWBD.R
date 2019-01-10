#' @title Find Watershed Boundary Geometries (WBD/HUC)
#' @description The United States Geological Survey maintains a hierarchical system of hydrologic units each assigned a
#' unique code (HUC). The hierarchical level is described by the number of digits in the code. A two-digit code (eg. HUC 2) is the coarsest unit of aggregation while the HUC 12 is the finest resolution.
#' The spatial geometries of these units are stored in the Watershed Boundary Dataset with coverage of the United States.
#' \code{findWBD} returns a \code{SpatialPolygonsDataFrame*} of WBD boundaries for the specified level within an AOI. Pending the query,
#' data comes from the USGS CIDA server or the USGS staged products FTP.
#'
#' Below you can see the general factors for each HUC level:\
#' \tabular{ccccc}{
#'   Name \tab Digits \tab Average Size (sqmiles) \tab Example Name \tab Example Code \cr
#'   Region \tab 2 \tab 177,560 \tab Pacific Northwest \tab 17 \cr
#'   Subregion \tab 4 \tab  16,800  \tab Lower Snake \tab 1706\cr
#'   Basin \tab 6 \tab 10,596  \tab Lower Snake \tab 170601 \cr
#'   Subbasin \tab 8 \tab 700  \tab Imnaha River \tab 17060102 \cr
#'   Watershed \tab 10 \tab 227 \tab Upper Imnaha River \tab 1706010201 \cr
#'   Subwatershed \tab 12 \tab 40 \tab North Fork Imnaha River \tab 170601020101 \cr
#' }
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param level defines the HUC level of interest (default = 8)
#' @param subbasins If TRUE, all subbasins of the supplied level will be joined to retuned list
#' @param crop If TRUE, all objects are cropped to the AOI boundaries (default = \code{TRUE})
#' @param ids  If TRUE,  a vector of finest resolution HUC codes is added to returned list (default = \code{FALSE})
#' @export
#' @examples
#' \dontrun{
#' # Get Cropped HUC8s for AOI
#'  getAOI(list("UCSB", 10, 10)) %>% findWBD()
#'
#'# Get Cropped HUC10s for AOI
#'  getAOI(list("UCSB", 10, 10)) %>% findWBD(level = 10)
#'
#'# Get Cropped HUC8s, HUC10s and HUC12s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 8, subbasins = TRUE)
#'
#'# Get uncropped HUC10s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 10, crop = FALSE)
#' }
#' @author
#' Mike Johnson

findWBD = function(AOI,
                   level = 8,
                   subbasins = FALSE,
                   crop = TRUE,
                   ids = FALSE){

  `%+%` = crayon::`%+%`

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  if(subbasins) { level =  seq(level, 12, 2) }

  bb = AOI::bbox_st(sp::spTransform(AOI$AOI, '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs'))
  bb.ordered =  paste(bb$xmin,bb$ymin,bb$xmax,bb$ymax, sep = "%2C")

  ref = data.frame(huc = seq(2,12,2), endpoint = 1:6)

  for(i in 1:length(level)){

    cat(crayon::white(paste0("Downloading (", i, "/", length(level), "): ")) %+% crayon::yellow(paste0("HUC", level[i])), "\n")

    endpoint = ref[which(ref$huc == level[i]),]$endpoint

    url = paste0('https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/',
                 endpoint,
                 '/query?',
                 '&geometry=',
                 bb.ordered,
                 '&geometryType=esriGeometryEnvelope',
                 '&outFields=*',
                 '&returnGeometry=true',
                 '&returnZ=false',
                 '&returnM=false',
                 '&returnExtentOnly=false',
                 '&f=geoJSON')

    shp = tryCatch({ sf::read_sf(url) },
                   warning = function(w) { NULL },
                   error = function(e) { NULL })

    if(is.null(shp)){

      cat(crayon::white("\nQuery to large for REST Service...\nReverting to direct download...\nPlease be patient...\n\n"))
      AOI = HydroData:::findWBD_staged(AOI = AOI, level = level, crop = crop)
      return(report.wbd(AOI, ids = ids, crop = crop, level = level))

    } else {

      AOI[[paste0("huc", level[i])]] = sf::st_transform(shp, as.character(AOI::aoiProj)) %>% cropHUC(AOI = AOI$AOI, crop = crop)

    }

  }

  gc()
  return(report.wbd(AOI, ids = ids, crop = crop, level = level))
}


findWBD_staged = function(AOI,
                          level = 8,
                          subbasins = FALSE,
                          crop = TRUE) {

  `%+%` = crayon::`%+%`
  data   = list()
  series = list()

  if (!(class(AOI) %in% c("list", "HydroData"))) { AOI = list(AOI = AOI) }

  huc_8 = query_cida(AOI$AOI, type = 'huc8', spatial  = FALSE)

  for(i in seq_along(level)) {
    AOI[[paste0("huc", level[i])]] = getWBD_raw(huc_8 = huc_8, level = level[i] ) %>% cropHUC(AOI = AOI$AOI, crop = crop)
  }

  unlink(list.files(tempdir(),pattern = "NHD", full.names = TRUE))
  gc()

  return(AOI)
}


getWBD_raw = function(huc_8, level, crop = TRUE){

  vals = substring(huc_8$huc8,1,level)
  HUC8 =  huc_8$huc8[!duplicated(vals)]

  `%+%` = crayon::`%+%`
  hucs = list()

  if(level == 8 ){ tmp = huc_8} else{

    urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",  HUC8 , "_HU8_Shape.zip")

    for (i in seq_along(urls)) {

      cat(crayon::white(paste0("Processing (", i, "/", length(urls), "): ")) %+% crayon::yellow(paste0("Huc", sprintf("%02d", level), " ", substring(HUC8[i], 1, level)), "\n"))

      td <-  tempfile()
      xx =  download.url(urls[i])

      unzip(xx$destfile, exdir = td, overwrite = TRUE)

      shp = sf::read_sf(paste0(td, '/Shape/WBDHU', level, ".shp")) %>% sf::st_transform(aoiProj)
      if(NROW(shp) != 0){ hucs[[paste0("huc_", level, "_", i)]] = sf::as_Spatial(shp) }
    }

    all.files = unlist(hucs)

    for (j in seq_along(level)) {
      tmp = do.call(rbind, all.files[grepl(pattern = paste0("huc_", level[j]), names(all.files))])
      tmp = tmp[!duplicated(eval(parse(text = paste0('tmp$HUC', level[j] )))),]
    }
  }

  return(tmp)

}



cropHUC = function(input, AOI, crop = TRUE){

  if(AOI::checkClass(input, 'sp')){ input = sf::st_as_sf(input)}

  shp = if(crop){
    suppressWarnings(suppressMessages( sf::st_intersection(input, sf::st_as_sf(AOI))))
  } else {
    suppressWarnings(suppressMessages(input[sf::st_as_sf(AOI), ]))
  }

  return(sf::as_Spatial(shp))
}

report.wbd = function(AOI, ids = ids, crop, level = NULL){

  tmp = names(AOI)[grep("huc", names(AOI))]

  if(length(tmp) > 1){ tmp = tmp[which.max(as.numeric(gsub("huc", "", tmp)))] }

  report = paste0(if(crop){"Cropped "} else {"Full "},   paste0("HUC", level, collapse = ", "), " boundaries")

  AOI = HydroData::return.what(AOI, type = tmp, report, vals = if(ids){toupper(tmp)})

  return(AOI)

}

