#' Get Snotel Data
#'
#'

getSnotel = function(state = NULL, county = NULL, clip_unit = NULL, IDs = NULL){

vals = NULL

if (any(!is.null(state), !is.null(county), !is.null(clip_unit))) {
   hope = suppressMessages( findSnotel(clip_unit = clip_unit, ids = T) )
   message("AOI defined.\n", length(hope$snotel), " Stations found.\nBegin downloading Snotel Data...\n" )
   IDs = hope$ids
}

for(i in seq_along(IDs)){

url = paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/",
             IDs[i],
             ":CA:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value")

val.temp = tryCatch({
  data.table::fread( url, skip = 58, header = T, showProgress = F)
}, error = function(e){
  val.temp = NULL
  message("     No data for station ", IDs[i])})

if(!is.null(val.temp)){
  val.temp$agency_cd = 'NRCS'
  val.temp$site_no = IDs[i]
  val.temp = data.frame(val.temp, stringsAsFactors = F)
}

vals = rbind(vals, val.temp)

message(dim(vals)[1], " Snotel values for station ", IDs[i], " downloaded (", i, "/", length(IDs), ")" )
}

vals$Date = as.Date(vals$Date, format ='%Y-%m-%d' )

names(vals) = c("Date", "SWE", "PRCP.ACC", "TMAX", "TMIN", "TAVG", "PRCP.INC", "agency_cd", "site_no")

vals = vals[c("agency_cd", "site_no", "Date", "SWE", "PRCP.ACC", "TMAX", "TMIN", "TAVG", "PRCP.INC")]

vals = data.frame(vals, stringsAsFactors = F)

return(vals)

}

