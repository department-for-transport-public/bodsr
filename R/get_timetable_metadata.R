##Get return from timetable API

#' @name get_timetable_metadata
#' @title Return timetable metadata from the BODS API
#' @export
#'
#' @param api_key API key for the BODS dataset passed as a string. Can be obtained from \link(https://data.bus-data.dft.gov.uk/api/)
#' @param limit integer. Maximum number of records to return for a query. Defaults to 25
#'
#' To return a lookup of current valid ATCO codes, use the lookup_atco_codes() function.
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @return Returns a data frame of timetable metadata including links to data from the BODS API.

#Function to pull in metadata
get_timetable_metadata <- function(api_key,
                                   limit = 25) {

  ##Check data values received
  if(!is.integer(limit)){
    stop("Please provide an integer value to the limit argument")
  }
  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/dataset?limit=",
                limit,
                "&api_key=",
                api_key)

  #Raw content from api
  download <- httr::GET(url)

  ##Return error message if authentication failed
  if(http_status(download)$reason == "Unathorized"){
    stop("Authentication credentials are not valid; please check you are using a valid BODS API key")
  }

  if(http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  }

  data <- jsonlite::fromJSON(
    httr::content(download, as = "text", encoding = "UTF-8"))$results

  return(data)
}
