findACIS <- function (AOI, param=NULL) {

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  load("/Users/mikejohnson/Documents/GitHub/ACISmeta.rda")

  bbox = AOI$AOI %>% bbox_st()

  if (is.null(param)) { param <- meta$element$code[1:7] }

  URL = paste0( "http://data.rcc-acis.org/StnMeta",
                "?elems=", paste(param, collapse = ","),
                "&bbox=",  paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep = ","),
                "&meta=",  paste('uid', 'name', 'state', 'll', 'elev', 'valid_daterange', 'sids', sep= ",")
                )

  base <- jsonlite::fromJSON(URL)

  dat = data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = F)

    names(dat) = c("uid", "name", "lon", "lat", "state", "elev")

    for(i in 1:NROW(base$meta)){

      tmp = as.data.frame(t(unlist(base$meta[i,])), stringsAsFactors = F)

      dat[[i, "uid"]] = tmp$uid
      dat[[i, "name"]] = tmp$name
      dat[[i, "lon"]] = tmp$ll1
      dat[[i, "lat"]] = tmp$ll2
      dat[[i, "state"]] = tmp$state
      dat[[i, "elev"]] = tmp$elev

      sids = tmp[grepl("sids", names(tmp))]

      for(j in seq_along(sids)){
        val = unlist(strsplit(as.character(sids[j]), " "))
        dat[i, paste0("sid", j)] = val[1]
        dat[i, paste0("sid_type", j)] = meta$stationIdType$description[meta$stationIdType$code == as.numeric(val[2])]
      }

    dat[i, "minDate"] = as.Date(min(unlist(tmp[grepl("date", names(tmp))])), "%Y-%m-%d")
    dat[i, "maxDate"] = as.Date(max(unlist(tmp[grepl("date", names(tmp))])), "%Y-%m-%d")
    }

    dat = sp::SpatialPointsDataFrame(coords = cbind(as.numeric(dat$lon), as.numeric(dat$lat)), data = dat)

    AOI[["acis"]] = dat

    return(AOI)

}


