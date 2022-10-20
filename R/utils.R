#' @name not_null
#' @title Join together a value and an associated API string if the value
#' is not NULL
not_null <- function(obj, string) {

  if(!is.null(obj)) {
    paste0(string, "=", obj, "&")
  } else{
    NULL
  }
}
