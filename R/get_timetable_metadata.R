##Return timetable metadata table from the BODS API, including urls of full datasets

#' @name get_timetable_metadata
#' @title Return timetable metadata from the BODS API
#' @export
#'
#' @param api_key API key for the BODS dataset passed as a string. Can be obtained from \link(https://data.bus-data.dft.gov.uk/api/)
#' @param limit integer. Maximum number of records to return for a query. Defaults to 25
#' @param search string to search records on; can be a value or partial value to
#' match the data set name, data set description, organisation name, or admin
#' area name.
#'
#' @importFrom httr GET content http_status
#' @importFrom jsonlite fromJSON
#'
#' @return Returns a data frame of timetable metadata including links to data from the BODS API.

#Function to pull in metadata
get_timetable_metadata <- function(api_key = Sys.getenv("BODS_KEY"),
                                   limit = 25,
                                   search = NULL) {

  ##Check data values received
  if(!is.numeric(limit)){
    stop("Please provide an integer value to the limit argument")
  }

  ##Use search string if it's not null
  if(!is.null(search)) {

    ##Swap spaces for character
    search <- paste0("search=", gsub(" ", "%20", search), "&")

  }

  #Paste together URL for API
  url <- paste0("https://data.bus-data.dft.gov.uk/api/v1/dataset?limit=",
                limit,
                "&",
                search,
                "api_key=",
                api_key)

  #Raw content from api
  download <- httr::GET(url)

  ##Return error message if authentication failed
  if(httr::http_status(download)$reason == "Unathorized"){
    stop("Authentication credentials are not valid; please check you are using a valid BODS API key")
  }

  if(httr::http_status(download)$reason == "Bad Request"){
    stop("Bad request; please check you have passed arguments to the function correctly")
  }

  data <- jsonlite::fromJSON(
    httr::content(download, as = "text", encoding = "UTF-8"))$results

  message(paste("Returning", nrow(data), "records"))

  return(data)
}

