save.file = function(data = NULL,
                     state = NULL,
                     county = NULL,
                     clip_unit = NULL,
                     agency = NULL,
                     source = NULL,
                     dataset = NULL,
                     other = NULL) {


  #### Define AOI name ###
  if (!is.null(county) & length(county) < 3) {
    AOI   = paste0(paste0(gsub(" ", "", county), collapse = "_"),
                   if (!is.null(county)) {
                     "_"
                   },
                   paste0(state, collapse = "_"))
  } else if (!is.null(state) & length(state) < 3) {
    AOI = paste0(state, collapse = "_")
  } else if (!is.null(clip_unit)) {
    if (class(clip_unit[[1]]) == 'character') {
      AOI = clip_unit[[1]]

      #AOI = clip_unit[[1]]
      #AOI = suppressMessages( ggmap::geocode(AOI, output = 'more', messaging = FALSE) )
      #AOI = paste0(gsub(" ","", AOI$administrative_area_level_2), "_", gsub(" ","",AOI$administrative_area_level_1), collapse = '_')
    }

    if (class(clip_unit[[1]]) == 'numeric') {
      AOI = suppressMessages(ggmap::revgeocode(
        location = c(clip_unit[[2]], clip_unit[[1]]),
        output = 'more'
      ))
      AOI = paste0(
        gsub(" ", "", AOI$administrative_area_level_2),
        "_",
        gsub(" ", "", AOI$administrative_area_level_1),
        collapse = '_'
      )
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

  # Save basemap if applicable
  if (!is.null(data$basemap)) {
    bmap = data$basemap
    bmap.file = paste0(raw.dir, "/", AOI, "/", "boundary", '/basemap.tif')
    if (!file.exists(bmap.file)) {
      if (grepl("raster", class(bmap)[1], ignore.case = TRUE)) {
        raster::writeRaster(bmap, bmap.file, options = c('TFW=YES'))
      }
    }
    data$basemap = NULL
  }

  # Save boundary file if applicable
  if (!is.null(data$clip)) {
    bound = data$clip
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
          layer = "clip",
          driver = "ESRI Shapefile"
        )
      }
    }
    data$clip = NULL
  }

  # Save boundary file if applicable
  if (!is.null(data$fiat)) {
    bound = data$fiat
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
          layer = "fiat",
          driver = "ESRI Shapefile"
        )
      }
    }
    data$fiat = NULL
  }

  # Save data
  for (i in 1:length(data)) {
    if (!is.null(data[[i]])) {
      fin = data[[i]]

      spatial.file = paste0(raw.dir, "/", AOI, "/", agency, "/", source)
      raster.file = paste0(raw.dir,
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
                           dataset,
                           ".tif")

      if (!file.exists(paste0(spatial.file, "/", dataset, ".shp")) &
          grepl("spatial", class(fin)[1], ignore.case = TRUE)) {
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
          layer = dataset,
          driver = "ESRI Shapefile"
        )
      }

      if (!file.exists(paste0(raster.file)) &
          grepl("raster", class(fin)[1], ignore.case = TRUE)) {
        raster::writeRaster(fin, raster.file, options = c('TFW=YES'))
      }
    }
  }
}
