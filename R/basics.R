##Function to pull in metadata
get_timetable_metadata <- function(api_key, limit = 25) {

  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/dataset?limit=",
                limit,
                "&api_key=",
                api_key)

  download <- httr::GET(url)


  ##Return err message if one or more atco codes are invalid
 # if(http_status(download)

  data <- jsonlite::fromJSON(
    httr::content(download, as = "text", encoding = "UTF-8"))$results

  return(data)
}


