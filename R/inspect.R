#' Inspect Timeseries data found with HydroData
#'
#' \code{inspect} can be used to explore timeseries HydroData objects
#' Namely:
#'  getUSGS
#'  getWU
#'  getNWM
#'  getGHCN
#'  getDaymet_point
#'
#' @param data data source to visualize
#' @param timestep time step to aggregate to, options include "daily", "monthly", "yearly"
#' @param param the parameter to visualize, must match a column name in the data input
#' @param save should the HTML dygraph be save to the users disk. Default FALSE
#'
#' @family HydroData 'viz' functions
#'
#' @export
#' @author Mike Johnson


inspect = function(data = NULL,
                   timestep = 'daily',
                   param = NULL,
                   save = FALSE) {

  param = tolower(param)
  names(data) = tolower(names(data))

  if (class(data) == 'list') {

    test3 = data[[eval(tolower(param))]]
    names(test3) = tolower(names(test3))

  } else {
    test3 = data
    names(test3) = tolower(names(test3))
  }

  dates = seq.Date(
    from = min(test3$date),
    to = max(test3$date),
    by = 'day'
  )
  vals = NULL

  if (timestep == 'daily') {
    dates = tail(dates, n = 3650 / 2)
    vals = test3[(test3$date %in% dates), ] %>% dplyr::select(-agency_cd)
    vals$date.agg = vals$date
    vals = vals %>% dplyr::select(-date)
  }

  if (timestep == 'monthly') {
    test3 = test3 %>% mutate(Month = months(test3$date),
                             Year = format(test3$date, format = "%Y"))

    for (i in seq_along(unique(test3$site_no))) {
      df = test3[test3$site_no == unique(test3$site_no)[i], ]
      vals.temp = aggregate(eval(get(param)) ~ Month + Year, df, FUN = mean)
      vals.temp = vals.temp %>% mutate(
        date.agg = zoo::as.yearmon(paste(vals.temp$Month, vals.temp$Year), format = "%B %Y"),
        site_no = unique(test3$site_no)[i]
      ) %>%
        dplyr::select(-Month,-Year)
      vals = rbind(vals, vals.temp)
    }

    names(vals) <-  c(param, 'date.agg', 'site_no')
  }

  if (timestep == 'yearly') {
    #dates =  tail(unique(format(dates,format="%Y")), n = 3650)

    test3 =  test3 %>% mutate(Year = format(test3$date, "%Y"))

    for (i in seq_along(unique(test3$site_no))) {
      df = test3[test3$site_no == unique(test3$site_no)[i], ]
      vals.temp = aggregate(df[, eval(param)] ~ Year, df, FUN = mean)
      vals.temp = vals.temp %>% mutate(
        site_no = unique(test3$site_no)[i],
        date.agg = as.Date(paste(Year, 12, 31, sep = "-"), format = "%Y-%m-%d")
      ) %>% select(-Year) %>% mutate(site_no = unique(test3$site_no)[i])


      vals = rbind(vals, vals.temp)

    }

    names(vals) <-  c(param,  'site_no', 'date.agg')
  }

  for (i in seq_along(unique(vals$site_no))) {
    message(
      "Station ",
      unique(vals$site_no)[i] ,
      " has ",
      dim(vals[vals$site_no == unique(vals$site_no)[i], ])[1],
      " ",
      timestep ,
      " ",
      toupper(param),
      " observations."
    )
  }

  vals = vals %>% dplyr::select(site_no, eval(param), date.agg)

  hhh = reshape(
    data = vals,
    idvar = "date.agg",
    timevar = "site_no",
    direction = "wide"
  )

  names(hhh) = c("Date", unique(vals$site_no))


  hmm = xts::xts(hhh, order.by = hhh$Date, rm.na = T)

  axis = setAxis(data = test3, dates = vals$date.agg, param = param, timestep= timestep)

  t = dygraph(hmm,
              main = axis$title) %>%
    dyRangeSelector() %>%

    dyHighlight(
      highlightCircleSize = 4,
      highlightSeriesBackgroundAlpha = 0.2,
      highlightSeriesOpts = list(strokeWidth = 2),
      hideOnMouseOut = FALSE
    ) %>%
    dyAxis("y", label = axis$ylabel) %>%
    dyOptions(
      axisLineWidth = 1.5,
      drawGrid = FALSE,
      connectSeparatedPoints = TRUE,
      colors = RColorBrewer::brewer.pal(3, "Set2")
    ) %>%
    dyLegend(show = "always",
             hideOnMouseOut = FALSE,
             width = 400)

  if (save) {
    htmlwidgets::saveWidget(t, file = "TS.html")
  }

  print(t)
  return(t)
}





