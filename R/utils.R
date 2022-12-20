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
#' @param date R object to check whether it is NULL or not
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
