get_ip_loc = function(){
ret <- rjson::fromJSON(readLines('http://freegeoip.net/json/', warn=FALSE))
coord = c(ret$latitude, ret$longitude)
return(coord)
}

