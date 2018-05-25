#' set Axis
#'
#' @param data data input
#' @param dates dates present in data
#' @param param parameter to name
#' @param timestep timestep to aggregate to
#' @export


setAxis = function(data, dates,  param, timestep ){

  param = tolower(param)
  variable = NULL
  units    = NULL
  agency   = NULL
  stat     = NULL
  ref      = NULL


  ## Weather Underground

  if(data$agency_cd[1] == "WeatherUnderground"){

    if(grepl("^dp_", param)){ variable = "Dew Point";           units = '(C)';     agency = "WU"}
    if(grepl("^h_", param)){ variable = "Humidity";             units = '(%)';     agency = "WU"}
    if(grepl("^slp_", param)){ variable = "Sea Level Pressure"; units = '(hPa)';   agency = "WU"}
    if(grepl("^v_", param)){ variable = "Visibility";           units = '(km)';    agency = "WU"}
    if(grepl("^wind", param)){ variable = "Wind Speed";              units = '(km/hr)'; agency = "WU"}
    if(grepl("^ppt", param)){ variable = "Precipitation";      units = '(mm)';    agency = "WU"}

    if(grepl("_min", param)){ stat = "Minimum"}
    if(grepl("_avg", param)){ stat = "Average"}
    if(grepl("_max", param)){ stat = "Maximum"}
  }


  ## USGS

  if(data$agency_cd[1] == "USGS"){
    if(grepl("flow", param)){ variable = "Stream Flow"; units = '(cfs)'; agency = "NWIS"}
  }

  ## GHCN

  if(data$agency_cd[1] == "NOAA"){

  if(grepl("prcp", param)){ variable = "Precipitaiton"; units = '(1/10 mm)';      agency = "GHCN"}
  if(grepl("tmax", param)){ variable = "Maximum Temperture"; units = '(1/10 (C))'; agency = "GHCN"}
  if(grepl("tmin", param)){ variable = "Minimum Temperture"; units = '(1/10 (C))'; agency = "GHCN"}
  if(grepl("snow", param)){ variable = "Snowfall"; units = '(mm)';        agency = "GHCN"}
  if(grepl("snwd", param)){ variable = "Snow Depth"; units = '(mm)';        agency = "GHCN"}

  }

  ## DAYMET

  if(data$agency_cd[1] == "DAYMET"){

    if(grepl("prcp", param)){ variable = "Precipitaiton"; units = '(mm)';      agency = "DAYMET"}
    if(grepl("dayl", param)){ variable = "Day Length"; units = '(sec)'; agency = "DAYMET"}
    if(grepl("srad", param)){ variable = "Solar Radiation"; units = '(w/m2)'; agency = "DAYMET"}
    if(grepl("swe", param)){ variable = "Snow Water Equivilent"; units = '(kg/m2)';        agency = "DAYMET"}
    if(grepl("tmax", param)){ variable = "Maximum Air Temperature"; units = '(C)';        agency = "DAYMET"}
    if(grepl("tmin", param)){ variable = "Minimum Air Temperature"; units = '(C)';        agency = "DAYMET"}

  }

  # National Water Model ....

  #if(data$agency_cd[1] == "OWI"){

  #}

  #
  #
  #
  #

  # Snotel ....

  if(data$agency_cd[1] == "NRCS"){

    if(grepl("swe", param)){ variable = "Snow Water Equivalent"; units = '(in)'
    agency = "NRCS"
    ref = "Start of Day Values"}

    if(grepl("prcp.acc", param)){ variable = "Precipitation Accumulation"; units = '(in)'
    agency = "NRCS"
    ref = "Start of Day Values"}

    if(grepl("tmin", param)){ variable = "Minimum Air Temperture"; units = '(F)'; agency = "NRCS"}
    if(grepl("tmax", param)){ variable = "Maximum Air Temperature"; units = '(F)';        agency = "NRCS"}
    if(grepl("tavg", param)){ variable = "Average Air Temperature"; units = '(F)';        agency = "NRCS"}
    if(grepl("prcp.inc", param)){ variable = "Precipitation Increment"; units = '(in)';        agency = "NRCS"}
  }


  if(format(min(dates), "%Y") == format(max(dates), "%Y")){
    range = format(min(dates), "%Y")
  }else{
    range = paste0(format(min(dates), "%Y"),"-",format(max(dates), "%Y"))
  }

  t  = paste0(simpleCap(timestep)," " , agency, " ", stat, " ", variable, ":"," ", range )
  y = paste0(ref, stat, " ", variable, " ", units)


  return(list(title = t, ylabel = y))
}
