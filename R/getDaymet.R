getDaymet <- function(state = NULL, county = NULL, clip_unit = NULL, tile.ids = NULL, year = NULL,
                      parameters = NULL, aggregate = NULL, crop.data = FALSE)
{

  parameters <- tolower(parameters)
  all.parameters <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")

  if (!all(parameters %in% all.parameters))
  {
    bad.param <- parameters[!(parameters %in% all.parameters)]
    stop("'", paste(bad.param, collapse = "', '"), "'", if (length(bad.param) >
                                                            1)
    {
      paste(" are")
    } else
    {
      paste(" is")
    }, " not a valid parameter for daymet data. Valid options include: \n", paste(all.parameters,
                                                                                  collapse = "', '"), "'")
  }

  all.years <- 1980:(lubridate::year(Sys.Date()) - 1)

  if (!all(year %in% all.years))
  {
    bad.year <- year[!(year %in% all.years)]
    stop("'", paste(bad.year, collapse = "', '"), "'", if (length(bad.year) >
                                                           1)
    {
      paste(" are")
    } else
    {
      paste(" is")
    }, " not a valid parameter for daymet data. Valid year(s) include: \n'",
    paste(all.parameters, collapse = "', '"), "'")
  }


  daymet_tiles = HydroData::daymet_tiles

  if(!is.null(tile.ids)) {
      tile.ids = tile.ids
      A = daymet_tiles[daymet_tiles$TileID == tile.no, ]
      bad.ids = setdiff(tile.ids,daymet_tiles$TileID )
      if(length(bad.ids > 0)){stop( paste(bad.ids,collapse = ", "),
                                    if(length(bad.ids) > 1){" are not valid tile ids."
                                    } else {
                                      " is not a valid tile id. "
                                    })
        }
  }else{
    A <- getAOI(state = state, county = county, clip_unit = clip_unit)
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit),
          ". Now loading DAYMET data from NASA ...\n")
    AOI <- spTransform(A, daymet_tiles@proj4string)
    tile.ids <- daymet_tiles[AOI, ]$TileID
  }

  daymet.data <- list()

  message("AOI covers ", length(tile.ids), " daymet tiles ...")

  ###### BREAK DOWN BY PARAMETER #######

  # open temp directory
  td <- tempdir()
  files <- list.files(td, pattern = ".nc$", full.names = T)
  file.remove(files)

  for (k in seq_along(parameters))
  {

    message("Beginning download of ", parameters[k], " data... (", length(year) *
              length(tile.ids), " netcdf files needed)")
    urls <- vector(mode = "character")
    save <- list()

    # Build url's for all years and tile numbers
    for (y in seq_along(year))
    {

      for (t in seq_along(tile.ids))
      {
        urls <- append(urls, paste0("https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/tiles/",
                                    year[y], "/", tile.ids[t], "_", year[y], "/", parameters[k], ".nc"))
      }
    }

    # download all url files
    for (i in seq_along(urls))
    {
      file <- paste0(td, "/", parameters[k], "_", sprintf("%02d", i), ".nc")
      if (!file.exists(file))
      {
        download.file(url = urls[i], destfile = file, mode = "wb", quiet = T)
        message("File ", i, " of ", length(year) * length(tile.ids), " finished.")
      }
    }

    message("Mosaicing ", parameters[k], " data...")

    # find all NETCDF files in temp directory
    all.nc <- list.files(td, pattern = ".nc$", full.names = TRUE)
    all.files <- grep(all.nc, pattern = parameters[k], value = T)


    # Declare a list of needed rasters
    needed.rasters <- list()
    names.111 <- rep(year, each = length(tile.ids))
    value <- rep(tile.ids, length(names.111))

    # fill needed rasters list with croped data
    for (i in seq_along(all.files))
    {
      temp.rasters <- brick(all.files[[i]])
      bounds <- spTransform(A, temp.rasters@crs)
      if (crop.data)
      {
        temp.rasters <- crop(temp.rasters, bounds)
      }

      needed.rasters[[paste0(names.111[i], "_", parameters[k], "_tile", value[i])]] <- stack(temp.rasters)
    }

    final.stacks <- list()
    for (i in seq_along(tile.ids))
    {
      final.stacks[[paste0("stack_", i)]] <- stack(needed.rasters[grep(pattern = tile.ids[i],
                                                                       names(needed.rasters), value = F)])
    }
    dates <- names(final.stacks[[1]])

    rst <- final.stacks


    if (length(tile.ids) > 1)
    {
      names(rst) <- NULL
      rst$fun <- mean
      save <- do.call(mosaic, rst)
      names(save) <- dates
    } else
    {
      save <- rst
    }

    daymet.data[[parameters[k]]] <- stack(save)
    message(parameters[k], " data complete!")
    unlink(list.files(td, pattern = parameters[k]))
  }

  if (!is.null(aggregate))
  {

    agg.daymet <- list()
    n <- aggregate
    message("Aggregating data to ", n, " day timesteps.")

    if (!is.null(daymet.data$dayl))
    {
      agg.daymet[["dayl"]] <- stackApply(daymet.data$dayl, rep(1:dim(daymet.data$dayl)[3],
                                                               each = n), fun = mean)
    }
    if (!is.null(daymet.data$prcp))
    {
      agg.daymet[["prcp"]] <- stackApply(daymet.data$prcp, rep(1:dim(daymet.data$prcp)[3],
                                                               each = n), fun = sum)
    }
    if (!is.null(daymet.data$srad))
    {
      agg.daymet[["srad"]] <- stackApply(daymet.data$srad, rep(1:dim(daymet.data$srad)[3],
                                                               each = n), fun = mean)
    }
    if (!is.null(daymet.data$swe))
    {
      agg.daymet[["swe"]] <- stackApply(daymet.data$swe, rep(1:dim(daymet.data$swe)[3],
                                                             each = n), fun = sum)
    }
    if (!is.null(daymet.data$tmax))
    {
      agg.daymet[["tmax"]] <- stackApply(daymet.data$tmax, rep(1:dim(daymet.data$tmax)[3],
                                                               each = n), fun = mean)
    }
    if (!is.null(daymet.data$tmin))
    {
      agg.daymet[["tmin"]] <- stackApply(daymet.data$tmin, rep(1:dim(daymet.data$tmin)[3],
                                                               each = n), fun = mean)
    }
    if (!is.null(daymet.data$vp))
    {
      agg.daymet[["vp"]] <- stackApply(daymet.data$vp, rep(1:dim(daymet.data$vp)[3],
                                                           each = n), fun = mean)
    }

    daymet.data <- agg.daymet
  }

  message("Data downloaded for all years, tiles and parameters.")
  return(daymet.data)

}




