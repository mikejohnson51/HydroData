#' Find Watershed boundaries in AOI
#'
#' @param state
#' @param county
#' @param clip_unit
#' @param level
#' @param subbasins
#' @param HUC8
#'
#' @return
#' @export
#'
#' @examples
#'
findWS = function(state = NULL, county = NULL, clip_unit = NULL, level = 10, subbasins = FALSE, HUC8 = NULL){

  huc = list()
  urls = NULL
  td <-  tempfile()

  if(!is.null(HUC8)) {
    HUC8 = HUC8
  } else {
    AOI = getAOI(state = state, county = county, clip_unit = clip_unit)
    flow = findNHD(clip_unit = AOI, boundary = F)
    HUC8 = unique(substr(flow$flowlines$reachcode, 1, 8))
    rm(flow)
  }

  message("There are ", length(HUC8), " HUC8 units in this AOI.")

 if(subbasins){
   items =  seq(level,12, 2)
 } else {
    items = level
}



if(!(length(HUC8) > 1) ) {


  temp <-  tempfile(pattern = "WBD", fileext = ".zip")

  urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_HU8_Shape.zip")

  download.file(url = urls, destfile =  temp)
  unzip(temp, exdir = td, overwrite = TRUE)

for( i in seq_along(items)){
      shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', items[i],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
      huc[[paste0("huc_", items[i])]] = shp[AOI, ]
}
  unlink(temp, recursive = T)
  #unlink(td, recursive = T)

 } else {

    series  = list()
    urls = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_", HUC8 ,"_HU8_Shape.zip")

  for(i in seq_along(urls)){

    temp = tempfile(pattern = "WBD", fileext = ".zip")
      download.file(url =  urls[i], destfile =  temp, quiet = F)

      unzip(temp, exdir = td, overwrite = TRUE)

      for( j in seq_along(items)){
        shp = rgdal::readOGR(paste0(td,'/Shape/WBDHU', items[j],".shp"), stringsAsFactors = F, verbose = F) %>% spTransform(HydroDataProj)
        huc[[paste0("huc_", items[j])]] = shp[AOI, ]
      }

      series[[paste0('hu_',HUC8[i])]] = huc

    }


    all.files = unlist(series)

    for( j in seq_along(items)){
      huc[[paste0("huc_", items[j])]] <- do.call(rbind, all.files[grepl(pattern = paste0(items[j], "$"), names(all.files))])
    }
  }

  return(huc)

}

