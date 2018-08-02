revGeo <- function(lat, lng) {

latlngStr = paste(lng,lat, sep = ",")
connectStr <- paste0("http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode",
                     "?f=pjson&featureTypes=&location=",
                      latlngStr)

  ll = readLines(connectStr, warn = F)

  address = gsub(" ", "",gsub(".*: \"s*|\".*", "", ll[grepl("\"Address\":", ll)]))
  city    = gsub(" ", "",gsub(".*: \"s*|\".*", "", ll[grepl("\"City\":", ll)]))
  postal  = gsub(" ", "",gsub(".*: \"s*|\".*", "", ll[grepl("\"Postal\":", ll)]))
  subregion  = gsub(" ", "",gsub(".*: \"s*|\".*", "", ll[grepl("\"Subregion\":", ll)]))
  region  = gsub(" ", "",gsub(".*: \"s*|\".*", "", ll[grepl("\"Region\":", ll)]))
  locs = c(address, city, postal, subregion, region)

  for(i in seq_along(locs)){
  if(nchar(locs[i]) <= 1){ locs[i] <-NA}
  }

  name = paste(locs[!is.na(locs)][1:3], collapse = "_")

  return(name)

}



