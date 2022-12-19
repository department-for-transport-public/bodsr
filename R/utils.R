#' @name not_null
#' @title Join together a value and an associated API string if the value
#' is not NULL
#'
#' @param obj R object to check whether it is NULL or not
#' @param string API string to append to R object

not_null <- function(obj, string) {

  if(!is.null(obj)) {
    paste0(string, "=", obj, "&")
  } else{
    NULL
  }
}

#' @name not_null_date
#' @title Join together a date value and an associated API string if the value
#' is not NULL
#'
#' @param obj R object to check whether it is NULL or not
#' @param string API string to append to R object

not_null_date <- function(date, string) {

  if(!is.null(date)) {

    if(!class(date) %in% c("Date", "POSIXct")){
      stop("Object must be of the class DATE")
    }

    paste0(string,
           "=",
           ##Convert to the right date format
           gsub(":",
                "%3A",
                format(as.Date(date), "%Y-%m-%dT%H:%M:%S"),
                fixed = TRUE),
           "&")
  } else{
    NULL
  }
}

##List everything inside a remote zip file
zip_list <- function(url){

  ##Download to temp location
  folder <- tempfile()

  httr::GET(
    url = url,
    write_disk(folder, overwrite = TRUE)
  )

  ##Read in first file as an XML
  files <- utils::unzip(folder, list = TRUE)

  return(files)
}
