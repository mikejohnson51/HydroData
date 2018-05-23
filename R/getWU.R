getWU = function(airport_code = NULL, year = NULL, month = 1:12, day = NULL, type = NULL){

  type = 'monthly'
  airport_code = toupper(airport_code)

  ap = HydroData::ap

  if(nchar(airport_code) == 3) { airport_code = ap$ICAO[which(airport_code == ap$IATA)] }
  if(!(airport_code %in% ap$ICAO)) { stop("Airport code not found") }

  if((type %in% c("daily", "weekly", "monthly"))){
    type = simpleCap(type)
  } else { stop("'", type, "' ", "is not a valid type. Select from:\n (1) daily (2) weekly (3) monthly") }

  if( type == 'daily' ){ index = 5 } else { index = 4 }
  if( is.null(day) ){ day = 1 }

  data.names = c(
    "Day",
    "T_max",    "T_avg",    "T_min",    # Temperture         (C)
    "DP_max",   "DP_avg",   "DP_min",   # Dew Point          (C)
    "H_max",    "H_avg",    "H_min",    # Humidity           (%)
    "SLP_max",  "SLP_avg",  "SLP_min",  # Sea Level Pressure (hPa)
    "V_max",    "V_avg",    "V_min",    # Visibility         (km)
    "Wind_max", "Wind_avg", "Wind_min", # Wind               (km/hr)
    "PPT_tot",
    "Events" )

  df.airports = NULL

  for( i in seq_along(year)){
    for( j in seq_along(month)) {

      df = xml2::read_html(paste0("https://www.wunderground.com/history/airport/",
                                  toupper(airport_code), "/",
                                  year[i], "/", month[j], "/", day, "/",
                                  paste0(type, "History"),
                                  ".html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=")) %>%
        rvest::html_nodes("table") %>%
        .[[index]] %>%
        rvest::html_table()

      df = df[-1,]
      df[df == "-"] <- NA
      df[df == "T"] <- NA

      df = data.frame(
        Date = as.Date(paste0(year[i], "-", month[j], "-", df[,1] )),
        Year = year[i],
        Month = month[j],
        df)

      t = as.matrix(df)
      hope = data.frame(lapply(split(t, col(t)), type.convert, as.is = TRUE), stringsAsFactors = FALSE)
      df.airports = rbind(df.airports, hope)
      message( "Year ", year[i], " Month ", j,  " downloaded.")
    }
  }
  df.airports = df.airports %>% dplyr:: mutate(agency_cd = "WeatherUnderground", site_no = airport_code)
  names(df.airports) = c("Date", "Year", "Month", data.names, "agency_cd", "site_no")
  df.airports[,1] = as.Date(df.airports[,1])
  return(df.airports)
}


