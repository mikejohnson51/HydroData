#' @title Find Watershed Boundary Geometries (WBD/HUC)
#' @description The United States Geological Survey maintains a hierarchical system of hydrologic units each assigned a
#' unique code (HUC). The heirarchical level is described by the number of digits in the code. A two-diget code (eg. HUC 2) is the coarsest unit of aggregation while the HUC 12 is the finest resulution.
#' The spatail geometries of these units are stored in the Watershed Boundary Dataset with coverage of the United States.
#' \code{findWBD} returns a \code{SpatialPolygonsDataFrame*} of WBD boundaries for the specified level within an AOI. Pending the query,
#' data comes from the USGS CIDA server or the USGS staged products FTP.\cr
#'
#' Below you can see the general factors for each HUC level:\cr
#' \tabular{ccccc}{
#'   Name \tab Digits \tab Average Size (sqmiles) \tab Example Name \tab Example Code \cr
#'  Region \tab 2 \tab 177,560 \tab Pacific Northwest \tab 17 \cr
#' Subregion \tab 4 \tab  16,800  \tab Lower Snake \tab 1706\cr
#'  Basin \tab 6 \tab 10,596  \tab Lower Snake \tab 170601 \cr
#'   Subbasin \tab 8 \tab 700  \tab Imnaha River \tab 17060102 \cr
#'   Watershed \tab 10 \tab 227 \tab Upper Imnaha River \tab 1706010201 \cr
#'   Subwatershed \tab 12 \tab 40 \tab North Fork Imnaha River \tab 170601020101 \cr
#' }\cr
#' @param AOI  A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param level defines the HUC level of interest (default = 8)
#' @param subbasins If TRUE, all subbasins of the supplied level will be joined to retuned list
#' @param crop If TRUE, all objects are cropped to the AOI boundaries (default = \code{TRUE})
#' @param ids  If TRUE,  a vector of finest resolution HUC codes is added to retuned list (default = \code{FALSE})
#' @export
#' @examples
#' \dontrun{
#' # Get Cropped HUC8s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD()
#'
#'# Get Cropped HUC10s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 10)
#'
#'# Get Cropped HUC8s, HUC10s and HUC12s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 8, subbasins = TRUE)
#'
#'# Get uncropped HUC10s for AOI
#'  getAOI(clip = list("UCSB", 10, 10)) %>% findWBD(level = 10, crop = FALSE)
#' }
#' @author
#' Mike Johnson


findWBD = function(AOI = NULL,
                  level = 8,
                  subbasins = FALSE,
                  crop = TRUE,
                  ids = FALSE){

  `%+%` = crayon::`%+%`
  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  sp = list()
  td <-  tempfile()
  series  = list()

  if(subbasins) { level =  seq(level, 12, 2) }

  if (all(level == 8)) {
    sp[["huc8"]] = query_cida(AOI$AOI, "huc08", spatial = TRUE)
    shp = if(crop){
      suppressWarnings(suppressMessages( sf::st_intersection(sp, sf::st_as_sf(AOI$AOI)))) } else {
        suppressWarnings(suppressMessages(sp[sf::st_as_sf(AOI$AOI), ]))
      }

    cat(crayon::white("Returned object contains: ") %+% crayon::green(paste(length(sp$huc8), "HUC8 boundaries"), "\n"))
  } else if (all(level == 12)) {
    sp[["huc12"]] = query_cida(AOI$AOI, "huc12", spatial = TRUE)
    shp = if(crop){
      suppressWarnings(suppressMessages( sf::st_intersection(sp, sf::st_as_sf(AOI$AOI)))) } else {
        suppressWarnings(suppressMessages(sp[sf::st_as_sf(AOI$AOI), ]))
      }
    cat(crayon::white("Returned object contains: ") %+% crayon::green(if(crop){"Cropped "} else {"Full "}, paste(length(sp$huc12), "HUC12 boundaries"), "\n"))
  } else {

    flow = query_cida(AOI$AOI, type ="huc08")
    HUC8 = flow$huc8

    #message("There are ", length(HUC8), " HUC8 units in this AOI: ", paste(HUC8, collapse = ", "))

    urls = paste0(
      "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",
      HUC8 ,
      "_HU8_Shape.zip"
    )

      for (i in seq_along(urls)) {

        cat(crayon::white(paste0("Downloading (", i, "/", length(urls), "): ")) %+% crayon::yellow(basename(urls[i])), "\n")
        xx = download.url(urls[i])
        unzip(xx$destfile, exdir = td, overwrite = TRUE)

        for (j in seq_along(level)) {

        shp = sf::read_sf(paste0(td, '/Shape/WBDHU', level[j], ".shp"))
        shp <- shp[!duplicated(data.frame(shp$Name)),]
        shp = sf::st_transform(shp, as.character(AOI::aoiProj))
        shp = if(crop){
            suppressWarnings(suppressMessages( sf::st_intersection(shp, sf::st_as_sf(AOI$AOI)))) } else {
            suppressWarnings(suppressMessages(shp[sf::st_as_sf(AOI$AOI), ]))
              }
        sp[[paste0("huc", level[j])]] = sf::as_Spatial(shp)

        }

        series[[paste0('huc', HUC8[i])]] = sp

      }

      all.files = unlist(series)

      for (j in seq_along(level)) {
        sp[[paste0("huc", level[j])]] <-
          do.call(rbind, all.files[grepl(pattern = paste0(level[j], "$"), names(all.files))])
      }

AOI = c(AOI, sp)

tmp = names(AOI)[grep("huc", names(AOI))]

if(length(tmp) > 1){
 tmp = tmp[which.max(as.numeric(gsub("huc", "", tmp)))]
}

report = paste0( if(crop){"Cropped "} else {"Full "},   paste0("HUC", level, collapse = ", "), " boundaries")

AOI = return.what(AOI, type = tmp, report, vals = if(ids){tmp})

return(AOI)
}
}


