#' Get NED data for an Area of Interest
#'
#' Function to download elevation data for an area of interest
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with station data in a list
#' @param res resolution of NED data. 1 equals 1 arc second, 13 equals 1/3 arc second.
#'
#' @examples
#' Get 1 arc second elevation for San Fransisco County, CA
#'
#' harris.clim = find_ghcnd_stations(state = 'TX', county = 'Harris')
#' plot(harris.clim$basemap)
#' plot(harris.clim$boundary, add = TRUE)
#' plot(harris.clim$gchn, add = TRUE, pch = 16, col = 'blue')
#'
#' Get Station IDs
#'
#' IDs = test$stations$ID
#'
#' @author
#' Mike Johnson

get_ned = function(state = NULL, county = NULL, clip_unit = NULL, res = 1, keep.boundary = FALSE){

  if(!(res != 1 || res != 13)){stop("Resoultion must be either 1 (1 arc second) or 13 (1/3 arc second).\n  Please use one of those values.")}

  ######### 1. Define Area of Interst #########

  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = FALSE)
  bb = matrix(AOI@bbox , ncol = 2)

  # Defined by upper left Coordinates

  lon = head(sprintf("%03d", abs(seq(floor(bb[1,1]), ceiling(bb[1,2]), by = 1))), -1)
  lat = head(seq(ceiling(bb[2,2]), floor(bb[2,1]), by = -1), -1)
  mat = expand.grid(lat,lon)
  message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading", res, "arc second NED data...")

  ########## 2. Download Data ##########

  urls = vector()

  for(i in 1:dim(mat)[1]){
    urls[i] = paste0('https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/', res, '/IMG/n', mat[i,1],"w", mat[i,2],".zip")
  }

  if(length(urls) > 1){
    verb = 'are'
    noun = 'rasters'
  } else {
    verb = 'is'
    noun = 'raster'
  }

  message(paste("There", verb, length(urls), "NED", noun, "in this scene."))

  temp = tempfile()
  tempd = tempdir()

  for(i in 1:length(urls)){
    message(paste0("Downloading raster ", i, " of ", length(urls)))
    download.file(url = urls[i], destfile = temp, quiet = TRUE )
    unzip(temp, exdir = tempd, overwrite = TRUE)
    message(paste0("Finished downloading raster ", i, " of ", length(urls)))
  }

  ########## 3. Process Data ##########

  input.rasters <- lapply(list.files(tempd, pattern = ".img", full.names = TRUE), raster)
  bounds = spTransform(AOI, input.rasters[[1]]@crs)

  for(j in 1:length(input.rasters)){
    if(!is.null(intersect(extent(input.rasters[[j]]),extent(bounds)))){
      input.rasters[[j]] <- crop(input.rasters[[j]], bounds)
      message("Raster number ", j," Cropped.")
    } else {
      message("Raster ", j, " not needed")
    }
  }

  if(length(input.rasters) > 1){
    message("Mosaicing raster...")
    utils::flush.console()

    input.rasters$fun <- max
    input.rasters$na.rm <- TRUE

    mos = do.call(mosaic, input.rasters)

    gc()
  } else {
    mos = input.rasters[[1]]
  }

  unlink(temp)
  unlink(tempd)

  if(keep.boundary == TRUE){
    message("Returned object contains elevation raster and boundary shapefile")
    return(list(elev = mos, boundary = bounds))
  }else{
    message("Returned object contains elevevation raster")
    return(mos)
  }
}

sf.elev = get_ned(state = 'CA', county = 'san francisco',  res = 1, keep.boundary = TRUE)

plot(sf.elev)
