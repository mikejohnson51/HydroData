#
# sdate = '1980-10-29'
# edate = NULL
# param = 'ALL'
# timestep = "daily"
#
# AOI = getAOI("Colorado Springs")
#
# y = getDaymet(AOI, sdate, edate, param, timestep)
# rasterVis::levelplot(y$daymet_1980$prcp)
#
# #################
# #################
# #################
# #################
#
# getDaymet_subset = function(AOI, sdate, edate, param, timestep){
#
# if(is.null(edate)){ edate = sdate}
#
# if(nchar(sdate) == 4){
#   sdate = paste0(sdate, "-01-01")
#   edate = paste0(edate, "-12-31")
# }
#
# if(nchar(sdate) == 7){
#   days = numberOfDays(as.Date(paste0(edate, "-01")))
#   sdate = paste0(sdate, "-01")
#   edate = paste0(edate, "-", days)
# }
#
# if(nchar(sdate) == 10){
#     sdate = sdate
#     edate = edate
#  }
#
#
# if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}
# if(any(class(AOI) == "sf")){ AOI = as_Spatial(AOI) }
#
# max_edate = as.numeric(format(Sys.time(), "%Y")) - 1
#
# bb = AOI::bbox_st(AOI$AOI)
#
# timestep = tolower(timestep)
#
# if(timestep == "monthy"){
#     port = 1345
#   } else if(timestep == 'annual') {
#     port = 1345
#   } else {
#     port = 1328
#   }
#
#   if (as.numeric(substr(sdate,1,4)) < 1980){ stop("Start date preceeds valid data range!") }
#   if (as.numeric(substr(edate,1,4)) > max_edate){ stop("End date exceeds valid data range!") }
#
#   years = seq(format(as.Date(sdate), "%Y"), format(as.Date(edate), "%Y"), 1)
#
#   if (any(grepl("ALL", toupper(param)))) {
#     if (tolower(timestep) == "daily"){
#       param = c('vp','tmin','tmax','swe','srad','prcp','dayl')
#     } else {
#       param = c('vp','tmin','tmax','prcp')
#     }
#   }
#
#   rast = list()
#
#   for ( i in years){
#
#     tmp = list()
#
#     for (j in param) {
#       if (timestep != "daily") {
#         if (j == "prcp") {
#           prefix = paste0(substr(timestep, 1, 3), "ttl")
#         } else {
#           prefix = paste0(substr(timestep, 1, 3), "avg")
#         }
#
#         file = sprintf("daymet_v3_%s_%s_%s_na.nc4", j, prefix, i)
#       } else {
#         file = sprintf("%s/daymet_v3_%s_%s_na.nc4", i, j, i)
#       }
#
#       url = paste0("https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/",
#                    port, "/",
#                    file,
#                    '?var=lat&var=lon',
#                    "&var=", param[1],
#                    '&north=', bb$ymax,
#                    '&west=', bb$xmin,
#                    '&east=', bb$xmax,
#                    '&south=', bb$ymin,
#                    '&time_start=', sdate, 'T12%3A00%3A00Z',
#                    '&time_end=', edate, 'T12%3A00%3A00Z',
#                    '&timeStride=1&accept=netcdf')
#
#       destdir <- normalizePath(paste0(tempdir(), "/."))
#       destfile <- paste0(destdir, "/", paste0("daymet_v3_", param[1], "_", years[1], "_na.nc"))
#
#       httr::GET(url = url, httr::write_disk(destfile, overwrite = T), httr::progress(type = "down"))
#
#       b = suppressWarnings( raster::brick(destfile, stopIfNotEqualSpaced = FALSE) )
#       if(dim(b)[3] == 1) { b = b[[1]]}
#
#       tmp[[j]] = raster::projectRaster(b, crs = AOI$AOI@proj4string)
#    }
#
#     file.remove(destfile)
#     rast[[paste0("daymet_", i)]] = tmp
#   }
#
#   return(rast)
#
# }
#
#
#
#   numberOfDays <- function(date) {
#     m <- format(date, format="%m")
#
#     while (format(date, format="%m") == m) {
#       date <- date + 1
#     }
#
#     return(as.integer(format(date - 1, format="%d")))
#   }
