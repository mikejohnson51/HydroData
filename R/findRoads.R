#' Find TIGER roads within Area of Interest
#'
#' Function to locate all TIGER roads within an Area of Interest from the US Census Bureau
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#'\dontrun{
#' ep.roads = find_roads(state = 'CO', county = 'el paso', keep.boundary = TRUE, keep.basemap = TRUE)
#' plot(ep.roads$basemap)
#' plot(ep.roads$boundary, add = T, lwd = 5 )
#' plot(ep.roads$roads,    add = T )
#'}
#'
#' @export
#' @author
#' Mike Johnson

findRoads = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){

  tempd = tempdir()

  ########## 1. Define AOI ##########
    #do.call(file.remove, list(list.files(tempdir(), full.names = T)))
    items =  list()
    input.shp = list()
    report = vector(mode = 'character')
    A = getAOI(state = state, county = county, clip_unit = clip_unit)
    AOI = getFiatBoundary(clip_unit = A)

    FIP = AOI$geoid
    if(is.null(FIP)){ FIP = AOI$map$FIP }
    if(is.null(FIP)){ FIP = AOI$shp$FIP }
    FIP = sprintf("%05d", as.numeric(FIP))
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Loading TIGER roads database...\n")

  ########## 2. Download Data ##########

    urls = vector()

    for(i in 1:length(FIP)){
      urls[i] = paste0("https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_", FIP[i], "_roads.zip")
    }

    if(length(urls) > 1){ verb = 'are'; noun = 'files'} else {verb = 'is'; noun = 'file'}
      message(paste("There", verb, length(urls), "TIGER", noun, "in this scene."))

    for(i in seq_along(urls)){
      temp = tempfile(fileext = ".zip")
      message(paste0("Downloading file ", i, " of ", length(urls)))
      download.file(url = urls[i], destfile = temp, quiet = TRUE )
      unzip(temp, exdir = tempd, overwrite = TRUE)
      message(paste0("Finished downloading file ", i, " of ", length(urls)))
    }

  ########## 3. Process Data ##########

    all.files = list.files(tempd, pattern = ".shp$", full.names = TRUE)
    bounds =  raster::extent(A)

    message(" we have arrived here 1")

    for(j in seq_along(all.files)){
      input.shp[[j]] = rgdal::readOGR(all.files[j]) %>% spTransform(HydroDataProj)
      message(" we have arrived here 2")
      input.shp[[j]] <- raster::crop(x = input.shp[[j]],  y = bounds)
      message(" we have arrived here 3")
      message("Shapefile number ", j," Cropped.")
    }

    df <- do.call("rbind", input.shp)

    unlink(temp, recursive = T)
    unlink(tempd, recursive = T)

    items[['roads']] = df ; report = append(report, "Returned list includes: TIGER raods shapefile")

    if(length(report) > 1) {report[length(report)] = paste("and", tail(report, n = 1))}
      message(paste(report, collapse = ", "))

    if(save){
        save.file(data = items,
                  state = state,
                  county = county,
                  clip_unit = clip_unit,
                  agency  = 'USCensus',
                  source  = "TIGER",
                  dataset = "roads",
                  other   = NULL )
    }

    return(items)
}


#rds = findRoads(clip_unit = list("UCSB", 10, 10))
