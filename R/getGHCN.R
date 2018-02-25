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
                   years = NULL) {
  DATA = list()

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
    download.file(url, destfile = dest3)

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
              "not avaialable for stations...")
    }

    if (!is.null(years)) {
      daily = daily %>% filter(YEAR %in% years)
    }

    # Set missing values from -9999 to NA
    daily[daily == -9999] <- NA


## Separate by element
    param = p
    fin <- lapply(p, function(param) {
      ## Select parameters of interset
      test =   daily %>%
        dplyr::filter(PARAMETER == param) %>%
        dplyr::select_(quote(-PARAMETER))

      test = tidyr::gather(test, DAY, param,-site_no,-YEAR,-MONTH)
      test = data.frame(test)

      test2 = test %>%  dplyr::mutate(Date = as.Date(with(
        test, paste(YEAR, MONTH, DAY, sep = "-")
      ), "%Y-%m-%d"),
      agency_code = "NOAA") %>%
        dplyr::arrange(Date) # %>% dplyr::select(-YEAR,-MONTH,-DAY) %>%

      test2 = test2[!is.na(test2$Date),]
      test2 =  test2[c(1, 7, 6, 5)]
      names(test2) = c("site_no", "agency_cd", "Date" , tolower(eval(param)))
      test2 = test2[c("agency_cd", "site_no", "Date", tolower(eval(param)))]
      return(test2)
    })

    names(fin) <- paste0(p)
    return(fin)
}

