#' @title Write Data to disk
#' @description interal function used to write data to disk according to it meta.data found via HydroData::meta.data()
#' @param class objects class
#' @param dir directory
#' @param feature_name object id
#' @param ext shp or gpkg
#'
#' @return NULL
#' @export
#'


writeHD = function(AOI, class, dir, feature_name, ext = "shp") {

  data = eval(parse(text = paste0("AOI$", feature_name)))

  if (grepl("Spatial", class)) {

    if(ext == "shp"){
      ext = ".shp"
      driver = "ESRI Shapefile"
    } else {
      ext = ".gpkg"
      driver = "GPKG"
    }

    file = paste0(dir, "/", feature_name, ext)

    if (!file.exists(file)) {

      data = sf::st_as_sf(data)
      names(data) = abbreviate(names(data), 10)
      st_write(data, dsn = file, layer = feature_name,
               driver = driver, delete_dsn = TRUE, quiet = TRUE)

    }
  }

  if (grepl("raster", class)) {
    write
  }
  if (grepl("data.frame", class)) {

    write.csv(data, file)
  }

}
