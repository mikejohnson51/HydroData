#' CIDA Server Query
#'
#' @param AOI and AOI object generated with the AOI package, or a bounding box
#' @param type the WBD or nhdplus object to return
#' @param spatial return `sp` (default) if \code{FALSE} return `sf`
#'
#' @return a \code{Spatial} object
#' @export

query_cida = function(AOI = NULL, type, spatial = TRUE, filter = NULL){

  df = data.frame(server = c(rep("WBD", 2),
                             rep("nhdplus", 3),
                             rep("NWC", 3)),

                  type = c('huc8','huc12',
                           'nhd', 'catchments', 'waterbodies',
                           'gagesII','gagesII_basins', 'epa_basins'),

                  call =   c("huc08", "huc12",
                             "nhdflowline_network", "catchmentsp", "nhdwaterbody",
                             "gagesII", "gagesii_basins", "epa_basins"),

                  stringsAsFactors = F)

  if(!(type %in% df$type)) { stop(paste("Name not avalaile must be one of:", paste(df$type, collapse = ", "))) }

  call = df[which(df$type == type),]

  URL <- paste0("https://cida.usgs.gov/nwc/geoserver/", call$server, "/ows")

  startXML <- paste0('<?xml version="1.0"?>',
                     '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                     ' xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip"',
                     ' xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                     '<wfs:Query xmlns:feature="https://cida.usgs.gov/', call$server, '" typeName="feature:', call$call, '" srsName="EPSG:4269">',
                     '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">')

  if(!is.null(AOI)){

    bb = AOI::bbox_st(AOI)

    clipFilter =  paste0('<ogc:BBOX>',
                         '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                         '<gml:Envelope srsName="urn:x-ogc:def:crs:EPSG:4269">',
                         '<gml:lowerCorner>',bb$ymin," ",bb$xmin,'</gml:lowerCorner>',
                         '<gml:upperCorner>',bb$ymax," ",bb$xmax,'</gml:upperCorner>',
                         '</gml:Envelope>',
                         '</ogc:BBOX>'
    )
  } else { clipFilter = NULL}

  endXML = paste0('</ogc:Filter>',
                  '</wfs:Query>',
                  '</wfs:GetFeature>')


  filterXML = paste0(startXML,
                     if(!is.null(filter)){ filter },
                     clipFilter,
                     if(all(!is.null(filter), substring(filter, 1, 9) == '<ogc:And>')) { "</ogc:And>"},
                     endXML)

  dest = file.path(tempdir(), "spatial_query1.zip")
  file <- httr::POST(URL, body = filterXML, httr::write_disk(dest, overwrite=T))

  #lines = sf::read_sf(rawToChar(file$content))

   filePath <- tempdir()
   list.files(filePath)
   #suppressWarnings(
   unzip(dest, exdir = filePath)
  # #)

  myfile = list.files(filePath, pattern = call$call, full.names = TRUE)
  file.call = substr(basename(myfile[1]), 1, nchar(basename(myfile)) - 4)
  lines <- sf::read_sf(filePath, file.call)


  sl = tryCatch({sf::st_zm(lines)},
                error = function(e){
                  return(NULL)
                }, warning = function(w){
                  return(NULL)
                }
  )

  #file.remove(c(myfile, dest))

  if(any(is.null(sl), nrow(sl) ==0)) {
    sl = NULL
    warning("No features found in this AOI.")
  } else {
    sl = sf::st_transform(sl, as.character(AOI$AOI@proj4string))
    if(spatial){ sl = sf::as_Spatial(sl)
    }
    return(sl)
  }

}


