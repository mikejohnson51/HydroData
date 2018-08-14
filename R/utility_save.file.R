#' Save File
#'
#' @param AOI What data to write
#' @param path State abb
#' @param ext country call
#'
#' @export



saveHD = function(AOI = NULL, path = NULL, ext = 'shp') {

  if(ext == "shp"){
    ext = ".shp"
    driver = "ESRI Shapefile"
  } else {
    ext = ".gpkg"
    driver = "GPKG"
  }

  name = AOI::revgeocode(c(mean(AOI$AOI@bbox[2,]), mean(AOI$AOI@bbox[1,])))
  if(!is.null(name$match_addr)){name = name$match_addr} else {name = name[1]}

  if(is.null(path)){raw.dir = normalizePath(paste0("./HydroData/"), mustWork = FALSE)
  } else { raw.dir = normalizePath(path, mustWork = FALSE) }

  AOI = AOI %>% meta.data()

  # initialize HydroData folder
  if (!file.exists(raw.dir)) {
    dir.create(raw.dir, showWarnings = FALSE, recursive = TRUE)
  }

  # initialize name folder
  if (!file.exists(paste0(raw.dir, "/", name))) {
    dir.create(paste0(raw.dir, "/", name),
               showWarnings = FALSE,
               recursive = TRUE)
  }

  meta = AOI$meta.data

  # initialize all folders

  for(i in unique(meta$agency)){

    tmp.a = paste0(raw.dir, "/", name, "/", i)

    if (!file.exists(tmp.a)) { dir.create(tmp.a, showWarnings = FALSE,recursive = TRUE) }

    for(j in which(meta$agency == i)){

      dataset = meta[j,]$dataset
      class = meta[j,]$class
      feature_name = meta[j,]$feature_name

      tmp.b = paste0(raw.dir, "/", name, "/", i, "/", dataset)

      if (!file.exists(tmp.b)) {dir.create(tmp.b, showWarnings = FALSE, recursive = TRUE) }
      writeHD(class , dir = tmp.b, feature_name, ext = ext)
    }
  }

}



