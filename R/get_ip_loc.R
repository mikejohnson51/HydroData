#' A function for returning a users Lat, long from IP Address
#'
#' @examples
#' my.location = get_loc_ip()
#'
#' @return
#'
#' A numeric lat, long pair
#'
#' @author
#' Mike Johnson

get_ip_loc = function(){

  ret <- rjson::fromJSON(readLines('http://freegeoip.net/json/', warn=FALSE))

  coord = c(ret$latitude, ret$longitude)

return(coord)
}
