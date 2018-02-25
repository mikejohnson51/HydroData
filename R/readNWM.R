#' Hourly Value NWM Data Retrival
#'
#' Imports National Water Model streamflow data from the Hydroshare Data Explore or a folder of previously downloaded NETCDF files.
#'
#' @param comids used to identify NHD reaches to subset CONUS forecast. Can be given as a numeric vector, a AOI shapefile (use defineAOI()) or a shapefile of NHD flowlines
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD for data retrivial, YYYY-MM-DD-HH-MM-SS for folder of NETCDF files.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Only used with config = 'analysis_assim'
#' @param config character describing what forecast congiguration to access. Options are 'analysis_assim', 'short_range', 'medium_range', or 'long_range'
#' @param time numeric decribing time a forecast is made in UTC timezone. Must be between 0 and 23.
#' @param forecast numeric describing number of hours forcasted out from defined 'time'
#' @param keep.flowlines logical. If TRUE, the NHD shapefile will be returned with flow data (cannot be used if comids defined by vector)
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with flow data in a list
#' @param path character to folder of NETCDF files.
#' @param interval.hr logical. Hourly step between folder of NETCDF files
#'
#' @examples
#' \dontrun{
#' #Get NWM data for November 15 - 30, 2017 for the HUC8 surrondining UCSBs campus
#'
#' ucsb.flow = readNWM(comids = get_WBD(location = 'UCSB', level = 10),
#'                     startDate = "2017-11-15", endDate = "2017-11-20",
#'                     config = "analysis_assim", time = 0, forecast = 0,
#'                     keep.flowlines = T, keep.basemap = T)
#'
#' #Get data from download forecast of the 2015 Christmas Day flood in Tuscaloosa, AL:
#'
#' al.flow = readNWM(comids = define_AOI(clip_unit = list("National Water Center", 10, 10)),
#'                   path = "/RetroData_12.25.2015", startDate = "2015-12-25 00:00:00",
#'                   interval.hr = 1)
#'}
#' @export
#' @author
#' Mike Johnson

getNWM = function(comids, startDate, endDate = NULL, config = "analysis_assim", time = NULL,
                   forecast = NULL, keep.flowlines = FALSE, keep.basemap = FALSE, path = NULL, interval.hr = NULL){

  if(class(comids) == 'numeric'){
    comids = comids
    shp = NULL
  }else if(class(comids) == 'SpatialLinesDataFrame'){
    shp = comids
    comids = shp$comid
  }else if(class(comids) == 'SpatialPolygonsDataFrame' | class(comids) == "SpatialPolygons"){
    shp = findNHD(clip_unit = comids)
    comids = shp$comid
  }

if(!is.null(path)){ all.files = list.files(path, full.names = TRUE) } else {

  param = 'channel'

############################## ENSURE VALID REQUEST

if(config == 'analysis_assim' & as.Date(startDate) <= as.Date('2016-05-28')){stop("Data for 'analyis_assim' configuration only avialable after 2016-05-28")}
if(config != 'analysis_assim' & as.Date(startDate) < suppressWarnings(Sys.Date() - 41)){stop(gsub("(\\b[a-z])", "\\U\\1", tolower(gsub("_", " ", config)), perl=TRUE), " forecasts  archived for 41 days. Select date after ",
  Sys.Date() - 41, " or 'analysis_assim' configuration.")}
if(!is.null(endDate)){
  if(as.Date(endDate) > suppressWarnings(Sys.Date())){stop("End data given has not occurred yet.")}}
if(!(config %in% c("short_range", "long_range", "medium_range", "analysis_assim"))){stop("Forecast param must be either (1)'short_range', (2)'long_range', (3)'medium_range, (4)'analysis_assim'")}
if(!(param %in% c("land", "channel", "terrain", "reservoir"))){stop("param must be either (1)'channel', (2)'land', (3)'terrain, (4)'reservoir'")}
if(all(any(config == 'analysis_assim', config == 'short_range'), any(max(time) > 23, min(time) < 0))){stop("Time must be between 0 and 23 for ", tolower(gsub("_", " ", config)))}
if(all(config == 'medium_range', !(time %in% c(0,6,12,18)))){stop("Time must be between 0, 6, 12, or 18 for medium range")}

############################# DEFINE FILE LIST

filelist_hydro = paste0(config,"_", param, "_", gsub("-","", startDate),"_t",".json")

  if(!is.null(endDate) ){ endDate = paste0("&endDate=", endDate)} else {endDate = NULL}
  if(!is.null(time) ){ time = paste0("&time=", paste(time,collapse=","))} else {time = NULL}
  if(!is.null(param)){ param = paste0("&geom=",  paste(param,collapse=","))} else {param = NULL}

getFilelist = paste0("https://appsdev.hydroshare.org/apps/nwm-data-explorer/api/GetFileList/?",
                         "config=", config,
                         "&startDate=",startDate,
                         endDate,
                         time,
                         param)

download.file(url = getFilelist, destfile = filelist_hydro)

files = rjson::fromJSON(file=filelist_hydro)

if(!is.null(forecast)){
  if(config == "analysis_assim"){
    toMatch = c(sprintf('%02d', forecast))
  } else if(config == "short_range"){
    toMatch = c(sprintf('%03d', forecast))
  }
    files = unique(grep(paste0("tm", toMatch, collapse = "|"), files, value = TRUE))
  }

temp = tempdir()

for (i in 1:length(files)){download.file(url = paste0("https://appsdev.hydroshare.org/apps/nwm-data-explorer/api/GetFile?file=", files[i]), mode = "wb", destfile = paste0(temp, "/", files[i]))}

all.files = list.files(temp, pattern = ".nc$", full.names = TRUE)

}

nc <- nc_open(filename = all.files[1])

comids.all = nc$var$streamflow$dim[[1]]$vals

comids_of_value = comids[comids %in% comids.all]

start <- vector(mode = "numeric", length(comids_of_value))

for(i in 1:length(comids_of_value)){
  start[i] = which(comids.all == comids_of_value[i])}

nc_close(nc)

df = NULL

for (i in 1:length(all.files)) {

  nc = nc_open(filename = all.files[i])
  values = ncvar_get(nc, varid = "streamflow")

  if(!is.null(path)){

    for(j in 1:length(start)){
      df =  rbind(df, data.frame("NOAA",
                                 comids_of_value[j],
                                 (ymd_hms(startDate, tz = 'UTC') + ((((i-1) * interval.hr))*60*60)),
                                 values[start[j]],
                                 stringsAsFactors = FALSE))
    }

      }else{
        forecast_hour = substring(sub(".*tm", "", all.files[i]),1,2)

        for(j in 1:length(start)){
            df =  rbind(df, data.frame("NOAA",
                               comids_of_value[j],
                               as.POSIXct(gsub("_", " ", ncatt_get(nc, 0)$model_initialization_time)),
                               values[start[j]],
                               forecast_hour,
                               "UTC",
                               stringsAsFactors = FALSE))
                                 }
      }
   nc_close(nc)
}

if(!is.null(path)){colnames(df) = c("agency_cd", "comid", "dateTime", "cms")
} else {
colnames(df) = c("agency_cd", "comid", "dateTime", "cms", "Latency", "tz_cd")}

df = df[order(df$comid,df$dateTime),]



#### Returned

if(sum(keep.flowlines, keep.basemap) > 0){

  items =  list()
  items[['Data']] = df
  if(keep.flowlines & !is.null(shp)){items[['flowlines']] = shp}
  if(keep.basemap & !is.null(shp)){items[['basemap']] = suppressWarnings(dismo::gmap(shp, lonlat = TRUE))}

  mess = vector(mode = 'character')
  mess = append(mess, "Returned list includes: NWM data frame")

  if(keep.flowlines){mess = append(mess, "NHD shapefile")}
  if(keep.basemap){mess = append(mess, "basemap raster")}

  end.item = length(mess)
  last.item = mess[end.item]
  last.item = paste("and", last.item)
  mess[end.item] = last.item

  message(paste(mess, collapse = ", "))
  return(items)} else{
  return(df)
  }
}

