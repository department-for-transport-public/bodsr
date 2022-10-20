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

