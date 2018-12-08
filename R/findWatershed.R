findWatershed = function(pt, includeParameters = TRUE){

    id = findNearestCOMID(point = pt, n = 1)
    line = id$nhd@lines[[1]]@Lines[[1]]@coords
    line = tail(data.frame(line, stringsAsFactors = FALSE), 1)
    colnames(line) = c("lon", "lat")

    point = sf::st_as_sf(x = pt,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj)) %>% sf::as_Spatial()
    drainage = sf::st_as_sf(x = line,  coords = c("lon", "lat"), crs = as.character(AOI::aoiProj)) %>% sf::as_Spatial()
    state = AOI::states[drainage,]$state_abbr

    tmp = httr::GET(
      paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=",
      state,
      "&",
      'xlocation=', line$lon,
      '&ylocation=', line$lat ,
      "&crs=4326&includeparameters=", as.character(tolower(includeParameters)),
      "&includeflowtypes=false&includefeatures=true&simplify=true"), httr::progress(type = "down"))

    if(tmp$status_code == 200){
    res <- httr::content(tmp, as = "text", encoding = "UTF-8")
    out <- jsonlite::fromJSON(res, simplifyVector = FALSE)

    x = matrix(unlist(out$featurecollection[[2]]$feature$feature[[1]]$geometry$coordinates[[1]]), ncol = 2, byrow = TRUE)
    tt = lapply(out$parameters, as.data.frame, stringsAsFactors = FALSE)
    tt = do.call(rbind, tt)

    pol = sf::st_sfc(sf::st_polygon(list(as.matrix(x))))
    h = sf::st_sf(tt, pol, row.names = row.names(tt))

    return(list(basin = h, drainage = drainage, point = point))
    } else {
      stop("basin not delinimated!")
    }

}

