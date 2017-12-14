#' Find Synthetic Rating Curve Information
#'
#' Download and subset a synthetic rating curves for a set of NHD flowlines
#'
#' @param flowlines. A shapefile of NHD flowlines.
#'
#' @examples
#' al.rating = get.rating(flowlines = al.flows)
#'
#' Allows for internal nesting of functions:
#' This examples nests get_ip_loc(), find_flowlines(), and get.rating()
#'
#' ucsb.rating = get.rating(flowlines = find_flowlines(clip_unit = list(get_ip_loc(), 10, 10)))
#'
#' Or for a watershed
#'
#' ucsb.rating = get.rating(flowlines = find_flowlines(clip_unit = get_WBD(location = "UCSB", level =10)))
#'
#' @export
#' @author
#' Mike Johnson

get.rating = function(flowlines){

  HUC6 = unique(substr(flowlines$reachcode, 1, 6))
  comids = flowlines$comid
  message(length(flowlines)," flowlines intersect ", length(HUC6) , " HUC6 unit(s). Downloading synthetic rating curve file(s).")

  rating_curves = list()
  df = NULL
  temp = tempdir()

for(i in 1:length(HUC6)){
    file = paste0(temp,"/rating-curve-table",HUC6[i], ".csv")
  if(!file.exists(file)){
    URL = paste0("http://141.142.170.172/nfiedata/HUC6/", HUC6[i],"/", "hydroprop-fulltable-",HUC6[i], ".csv")
    download.file(url = URL, file)
  }else{message( ". . . ." )}
}

  HUC_files = list.files(temp, pattern = HUC6, full.names = TRUE)
  rating.files = grep(".csv$", HUC_files, value = TRUE)

 for (i in 1:length(rating.files)){

   build = as.data.frame(data.table::fread(rating.files[i]) %>% mutate(cms = `Discharge (m3s-1)`) %>% dplyr::select(CatchId,cms,Stage), showProgess = FALSE)
   build = build[build$CatchId %in% comids,]
   df = rbind(df, build)
 }


colnames(df) = c("comid", "cms", "stage")

return(df)

}

