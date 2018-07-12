#' CIDA Server Query
#'
#' @param AOI and AOI object generated with the AOI package, or a bounding box
#' @param type the WBD or nhdplus object to return
#' @param spatial return `sp` (default) if \code{FALSE} return `sf`
#'
#' @return a \code{Spatial} object
#' @export

query_cida = function(AOI, type, spatial = TRUE){

df = data.frame(server = c(rep("WBD", 2), rep("nhdplus", 2)),
                type = c("huc08", "huc12", "nhdflowline_network", "catchmentsp"), stringsAsFactors = F)

bb = AOI@bbox

if(!(type %in% df$type)){stop("Type not found.")}

server = df[which(df$type == type),1]

url_base <- paste0("https://cida.usgs.gov/nwc/geoserver/",
                   server,
                   "/ows",
                   "?service=WFS",
                   "&version=1.0.0",
                   "&request=GetFeature",
                   "&typeName=",
                   server, ":", type,
                   "&outputFormat=application%2Fjson",
                   "&srsName=EPSG:4269")

url <- paste0(url_base, "&bbox=",
              paste(bb[2,1], bb[1,1],
                    bb[2,2], bb[1,2],
                    "urn:ogc:def:crs:EPSG:4269", sep = ","))

sl = tryCatch({sf::st_zm(sf::read_sf(url))},
              error = function(e){
                return(NULL)
              }, warning = function(w){
                return(NULL)
              }
)

if(any(is.null(sl), nrow(sl) ==0)) {stop("O features found in this AOI.")}

if(spatial) {sl = sf::as_Spatial(sl)}

return(sl)

}

