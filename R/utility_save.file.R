#' Save File
#'
#' @param data What data to write
#' @param state State abb
#' @param county country call
#' @param clip_unit clip unit call
#' @param agency what agency maintains the data
#' @param source what is the source of the data
#' @param dataset what is the dataset that is subset
#' @param other anything else that should be documented (eg year)
#'
#' @export



save.file = function(data = NULL,
                     state = NULL,
                     county = NULL,
                     clip_unit = NULL,
                     agency = NULL,
                     source = NULL,
                     dataset = NULL,
                     other = NULL) {

  #### Define AOI name ###
  if (all(!is.null(county), length(county) < 3)) {
    AOI   = paste0(paste0(gsub(" ", " ", county), collapse = "_"),
                   if (!is.null(county)) {
                     "_"
                   },
                   paste0(state, collapse = "_"))
  } else if (all(!is.null(state), length(state) < 3)) {
    AOI = paste0(state, collapse = "_")

  } else if (!is.null(clip_unit)) {
    if (class(clip_unit[[1]]) == 'character') {
      AOI = paste(clip_unit[[1]], clip_unit[[2]], clip_unit[[3]], sep  = "_")
    } else{
      AOI = paste(round(clip_unit[[1]], 2),
                  round(clip_unit[[2]], 2),
                  clip_unit[[3]],
                  clip_unit[[4]],
                  sep  = "_")
    }
  }

  raw.dir = normalizePath(paste0("./HydroData/"), mustWork = FALSE)

  # initialize HydroData folder
  if (!file.exists(raw.dir)) {
    dir.create(raw.dir, showWarnings = FALSE, recursive = TRUE)
  }

  # initialize AOI folder
  if (!file.exists(paste0(raw.dir, "/", AOI))) {
    dir.create(paste0(raw.dir, "/", AOI),
               showWarnings = FALSE,
               recursive = TRUE)
  }

  # initialize boundary folder
  if (!file.exists(paste0(raw.dir, "/", AOI, "/", "boundary"))) {
    dir.create(
      paste0(raw.dir, "/", AOI, "/", "boundary"),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  # initialize agency folder
  if (!file.exists(paste0(raw.dir, "/", AOI, "/", agency))) {
    dir.create(
      paste0(raw.dir, "/", AOI, "/", agency),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  # initialize source folder
  if (!file.exists(paste0(raw.dir, "/", AOI, "/", agency, "/", source))) {
    dir.create(
      paste0(raw.dir, "/", AOI, "/", agency, "/", source),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  # Save boundary file if applicable
  if (!is.null(data$boundary)) {
    bound = data$boundary
    bound.file = paste0(raw.dir, "/", AOI, "/", "boundary")
    if (!file.exists(paste0(bound.file, "/boundary.shp"))) {
      if (grepl("spatialpolygons", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialPolygonsDataFrame")
      }
      if (grepl("spatiallines", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialLinesDataFrame")
      }
      if (grepl("spatialpoints", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialPointssDataFrame")
      }
      if (grepl("spatial", class(bound)[1], ignore.case = TRUE)) {
        rgdal::writeOGR(
          obj = bound,
          dsn = bound.file,
          layer = "boundary",
          driver = "ESRI Shapefile",
          overwrite_layer = TRUE
        )
      }
    }
    data$boundary = NULL
  }

  # Save Fiat file if applicable
  if (!is.null(data$fiat)) {
    bound = data$fiat
    bound.file = paste0(raw.dir, "/", AOI, "/", "boundary")
    if (!file.exists(paste0(bound.file, "/fiat.shp"))) {
      if (grepl("spatialpolygons", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialPolygonsDataFrame")
      }
      if (grepl("spatiallines", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialLinesDataFrame")
      }
      if (grepl("spatialpoints", class(bound)[1], ignore.case = TRUE)) {
        bound = as(bound, "SpatialPointssDataFrame")
      }
      if (grepl("spatial", class(bound)[1], ignore.case = TRUE)) {
        rgdal::writeOGR(
          obj = bound,
          dsn = bound.file,
          layer = "fiat",
          driver = "ESRI Shapefile",
          overwrite_layer = TRUE
        )
      }
    }
    data$fiat = NULL
  }

  data$ids = NULL

  # Save data
  for (i in 1:length(data)) {
    name = names(data)

    if (!is.null(data[[i]])) {
      fin = data[[i]]

      spatial.file = paste0(raw.dir, "/", AOI, "/", agency, "/", source)

      raster.file =  paste0(raw.dir,
                            "/",
                            AOI,
                            "/",
                            agency,
                            "/",
                            source,
                            "/",
                            if (!is.null(other)) {
                              paste0(other[i], "_")
                            },
                            name[i],
                            ".tif")

      timeseries.file = paste0(raw.dir, "/", AOI, "/", agency, "/", source)

      if (all(!file.exists(paste0(spatial.file, "/", name[[i]], ".shp")),
          grepl("spatial", class(fin)[1], ignore.case = TRUE))) {

        if (grepl("spatialpolygons", class(fin)[1], ignore.case = TRUE)) {
          fin = as(fin, "SpatialPolygonsDataFrame")
        }

        if (grepl("spatiallines", class(fin)[1], ignore.case = TRUE)) {
          fin = as(fin, "SpatialLinesDataFrame")
        }

        if (grepl("spatialpoints", class(fin)[1], ignore.case = TRUE)) {
          fin = as(fin, "SpatialPointsDataFrame")
        }

        rgdal::writeOGR(
          obj = fin,
          dsn = spatial.file,
          layer = name[[i]],
          driver = "ESRI Shapefile",
          overwrite_layer = TRUE
        )

      } else if (!file.exists(paste0(raster.file)) &
                 grepl("raster", class(fin)[1], ignore.case = TRUE)) {
        raster::writeRaster(fin, raster.file, options = c('TFW=YES'))
      } else if (class(data) == 'data.frame') {
        write.csv(data, file = paste0(timeseries.file, "/", source,"_", paste0(other, collapse = "_"),".csv"))
      }
    }
  }
}