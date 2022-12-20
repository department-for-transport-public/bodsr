#' @name extract_zip_or_xml
#' @title Open data from a single line metadata table where it's zip or xml format
#'
#' @param file A single row of table metadata extracted using get_timetable_metadata()
#'
#' @importFrom httr write_disk GET
#'
#' @return returns a dataframe of information extracted from the given xml or zip url

extract_zip_or_xml <- function(file){

  ##Try to unzip with names if it's a zip
  if(file$extension == "zip"){

    open_all_xml(file$url)

  } else if(file$extension == "xml"){

    ##Download and open xml file
    xml_loc <- tempfile(fileext = ".xml")

    httr::GET(
      url = file$url,
      httr::write_disk(xml_loc, overwrite = TRUE)
    )

    line_level_xml(xml_loc)

  }else{
    stop("Unsupported file type")
  }

}

#' @name get_timetable_data
#' @title Extract timetable data from all rows of the provided metadata table
#'
#' @param file A single row of table metadata extracted using get_timetable_metadata()
#' @param level A string specifying whether data returned should be at the
#' line or stop level. Options can be "line" or "stop"
#'
#' @importFrom httr write_disk GET
#' @export
#'
#' @return returns list of timetable dataframes, with each dataframe corresponding to a
#' row on the provided timetable metadata

get_timetable_data <- function(timetable_metadata, level = "line"){

  ##Extract metadata that applies to all files
  meta <- timetable_metadata %>%
    dplyr::select(url, dataSetID = id,
                  operatorName, description, status, extension, dqScore, dqRag)

  rowwise_extract <- function(i){
    message("Extracting row ", i, " of ", nrow(meta))

    x <- meta[i, ]

    x %>%
      ##Read in the xml
      extract_zip_or_xml() %>%
      ##Join to the URL deets
      bind_cols(x)
  }

  #Loop over all the rows
  purrr::map(.x = 1:nrow(meta),
             .f = rowwise_extract)
}

