##Return SIRI-VM XML location data table from the BODS API, including urls of full datasets

#' @name get_location_xml
#' @title Return XML location data from the BODS API
#' @export
#'
#' @param api_key API key for the BODS dataset passed as a string. Can be obtained from \link(https://data.bus-data.dft.gov.uk/api/)
#' @param bounding_box vector of four numerics. Limit results to fares data sets
#' that contain information for the area within the rectangular boundingBox
#' you set using co-ordinates [minLatitude, maxLatitude, minLongitude, maxLongitude].
#' Defaults to NULL.
#'
#' @importFrom httr GET content http_status
#' @importFrom jsonlite fromJSON
#'
#' @return Returns bus location data in XML SIRI-VM format. More detail on this format can be found \link(https://data.bus-data.dft.gov.uk/guidance/requirements/?section=dataformats)

#Function to pull in metadata
get_location_xml <- function(api_key = Sys.getenv("BODS_KEY")) {

  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/datafeed?",
                "boundingBox=51.401,51.509,0.01,0.201",
                "&api_key=", api_key)

  ##Read from url
  download <- httr::GET(url)


  ##Return error message if authentication failed
  if(httr::http_status(download)$reason == "Unathorized"){
    stop("Authentication credentials are not valid; please check you are using a valid BODS API key")
  }

  if(httr::http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  }

  ##Read xml
  xml2::read_xml(download)

}
