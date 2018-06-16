#' Get GHCN data from list of station IDs
#'
#' Function to download a dataframe of meterological data from known GHCN station. See findGHCN function to find station ids by AOI.
#'
#' @param IDs a list or single of station IDs
#' @param parameters a character string. Should the returned data be subset to a particular parameter(s)?
#' @param years  a character or numeric string. Should the returned data be subset to a particular year(s)?
#'
#' @author
#' Mike Johnson
#' @export

getGHCN = function(IDs = NULL,
                   parameters = NULL,
                   years = NULL,
                   save = FALSE) {


  if (is.null(parameters)) {
    p <- c("PRCP", "TMAX", "TMIN", "SNOW","SNWD")
  } else {
    p = toupper(parameters)
  }

  # Download and rbind all files ...

  # filter by Param
  daily = NULL

  for (i in seq_along(IDs)) {
    url <-
      paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",
            IDs[i],
            ".dly",
            sep = "")
    dest3 = tempfile()
    download.file(url, destfile = dest3, quiet = TRUE)

    daily.temp = readr::read_fwf(
      dest3,
      col_positions = readr::fwf_positions(
        start = c(1, 12, 16, 18, seq(22, 262, 8)),
        end = c(11, 15, 17, 21, seq(26, 266, 8)),
        col_names = c("site_no", "YEAR", "MONTH", "PARAMETER", paste0(1:31))
      ),
      col_types = paste0(c("cicc", rep("i", 31)), collapse = "")
    )

    daily = rbind(daily, daily.temp)
  }

  ## Check for Errors

  daily = daily %>% dplyr::filter(PARAMETER %in% toupper(p))


  missing.elements <- setdiff(toupper(p), unique(daily$PARAMETER))

  if (length(missing.elements) > 0){
    warning(paste(missing.elements, collapse = ", "),
            " not avaialable for stations...")
  }

  if (!is.null(years)) {
    daily = daily %>% filter(YEAR %in% years)
  }

  # Set missing values from -9999 to NA
  daily[daily == -9999] <- NA


  ## Separate by element
  param = p[!(p %in% missing.elements)]

  long = suppressWarnings( reshape(daily,
                                    idvar = c("site_no", "YEAR", "MONTH", "PARAMETER"),
                                    timevar = "DAY",
                                    v.names = "Value",
                                    varying = list(names(daily)[5:35]),
                                    direction = 'long',
                                    new.row.names = NULL)) %>%
    dplyr::mutate(Date = as.Date(with( daily, paste(YEAR, MONTH, DAY, sep = "-")), "%Y-%m-%d"),agency_code = "NOAA") %>%
    dplyr::arrange(PARAMETER, Date)  %>%
    dplyr::select(-YEAR,-MONTH,-DAY)

  wide <- reshape(as.data.frame(long), v.names = "Value", idvar = c("site_no", "Date"),
                  timevar = "PARAMETER", direction = "wide")

  wide = wide[!is.na(wide$Date),]

  colnames(wide) = c("site_no", "Date", "agency_cd", param)

  if(save){
      save.file(
        data = wide,
        state = state,
        county = county,
        clip_unit = clip_unit,
        agency  = 'NOAA',
        source  = "GHCN",
        dataset = "GCHN",
        other   = param
      )
    }

  return(wide)
}

