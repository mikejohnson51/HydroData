#' Return HydroData Object metadata
#'
#' @param AOI
#'
#' @return
#' @export
#'

meta.data = function(AOI){

  meta = data.frame(matrix(ncol = 9, nrow = 0), stringsAsFactors = F)

  names(meta) = c("type",
                  "agency",
                  "source",
                  "dataset",
                  "feature_name",
                  "feature_no",
                  "other",
                  "class",
                  "accessDate")


  if("nwis" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("Stream gages", "USGS", "CIDA", "NWIS", "nwis_gages", length(AOI$nwis), "NA", class(AOI$nwis)[1], as.character(Sys.Date()))
  }

  if("nhd" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("River Networks", "USGS", "CIDA", "NHD", "nhdflowlines", length(AOI$nhd), "NA", class(AOI$nhd)[1], as.character(Sys.Date()))
  }

  if("snotel" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("Snotel Stations", "USDA", "NRCS", "SNOTEL", "snotel_sta", length(AOI$snotel), "NA", class(AOI$snotel)[1], as.character(Sys.Date()))
  }

  if("ap" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("International Airports", "Open flights", "NCAR", "Airports", "ap", length(AOI$ap), "NA", class(AOI$ap)[1], as.character(Sys.Date()))
  }

  if("ghcn" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("Climate Stations", "NOAA", "GHCN", "GHCN", "ghcn", length(AOI$ghcn), "NA", class(AOI$ghcn)[1], as.character(Sys.Date()))
  }

  if("waterbodies" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("Waterbodies", "USGS", "CIDA", "NHD", "nhdwaterbodies", length(AOI$waterbodies), "NA", class(AOI$waterbodies)[1], as.character(Sys.Date()))
  }

  if("dams" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("Dams", "USACE", "NID", "dams", "niddams", length(AOI$dams), "NA", class(AOI$dams)[1], as.character(Sys.Date()))
  }

  if("gagesII" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("GagesII points", "USGS", "CIDA", "gagesII", "gagesII", length(AOI$gagesII), "NA", class(AOI$gagesII)[1], as.character(Sys.Date()))
  }

  if("gagesII_basins" %in% names(AOI)){
    meta[nrow(meta) + 1, ] = c("GagesII basins", "USGS", "CIDA", "GgagesII_basins", "gagesII_basins",  length(AOI$gagesII_basins), "NA", class(AOI$gagesII_basins)[1], as.character(Sys.Date()))
  }

  if(any(grepl("huc", names(AOI)))){
    tmp = names(AOI)[grepl("huc", names(AOI))]

    for(i in seq_along(tmp)){
      d = eval(parse(text = paste("AOI", tmp[i], sep = "$")))
      meta[nrow(meta) + 1, ] = c(tmp[i], "USGS", "CIDA", "WBD", tmp[i], length(d), "NA", class(d)[1], as.character(Sys.Date()))
    }
  }

  if(any(grepl("nlcd", names(AOI)))){
    tmp = names(AOI)[grepl("nlcd", names(AOI))]

    for(i in seq_along(tmp)){
      d = eval(parse(text = paste("AOI", tmp[i], sep = "$")))
      meta[nrow(meta) + 1, ] = c(tmp[i], "USGS", "MRLC", "NLCD", tmp[i], length(d), "NA", class(d)[1], as.character(Sys.Date()))
    }
  }

  AOI[["meta.data"]] = meta
  return(AOI)
}

