#' Download data from URL
#'
#' @param url the url
#' @param mode download mode
#' @param max.try number of attempts allowed
#'
#' @return
#' @export
#'

download.url = function(url, mode = "curl", max.try = 5){

  destdir <- normalizePath(paste0(tempdir(), "/."))
  destfile <- paste0(destdir, "/", basename(url))

  code = -9999
  rep = 0

  while(code != 0 & rep < max.try){
      rep = rep + 1
      code = utils::download.file(url = url, destfile = destfile, mode = "wb", quiet = FALSE)
      if(code != 0){ Sys.sleep(2) }
    }

  return(list(code = code, destfile = destfile))

}
