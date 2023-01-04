#' @name extract_stop_level_data
#' @title Open stop-level data from a single line metadata table where it's zip or xml format
#'
#' @param file A single row of table metadata extracted using get_timetable_metadata()
#'
#' @importFrom httr write_disk GET
#'
#' @return returns a dataframe of information extracted from the given xml or zip url

extract_stop_level_data <- function(file){

  ##Try to unzip with names if it's a zip
  if(file$extension == "zip"){

    open_all_xml(url = file$url, fun = stop_level_xml)

  } else if(file$extension == "xml"){

    ##Download and open xml file
    xml_loc <- tempfile(fileext = ".xml")

    httr::GET(
      url = file$url,
      httr::write_disk(xml_loc, overwrite = TRUE)
    )

    stop_level_xml(xml_loc, 1, 1)

  }else{
    stop("Unsupported file type")
  }

}

#' @name get_timetable_data
#' @title Extract timetable data from all rows of the provided metadata table
#'
#' @param timetable_metadata A single row of table metadata extracted using get_timetable_metadata()
#' @param level A string specifying whether data returned should be at the
#' bus line or individual stop level. Options can be "line" or "stop"
#'
#' @importFrom httr write_disk GET
#' @importFrom dplyr "%>%" bind_cols select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
#'
#' @return returns list of timetable dataframes, with each dataframe corresponding to a
#' row on the provided timetable metadata

get_timetable_data <- function(timetable_metadata, level = "line"){

  ##Extract metadata that applies to all files
  meta <- timetable_metadata %>%
    dplyr::select(.data$url, "dataSetID" = .data$id,
                  .data$operatorName, .data$description, .data$status,
                  .data$extension, .data$dqScore, .data$dqRag)

  rowwise_extract <- function(i){
    message("Extracting row ", i, " of ", nrow(meta))

    x <- meta[i, ]

    ##Select line or stop level data
    if(level == "line"){
      x %>%
        ##Read in the xml
        extract_line_level_data() %>%
        ##Join to the URL deets
        dplyr::bind_cols(x)

      } else if(level == "stop"){

      x %>%
        ##Read in the xml
        extract_stop_level_data() %>%
        ##Join to the URL deets
        dplyr::bind_cols(x)
    } else{
      stop("Level type not recognised. Accepted values are 'level' or 'stop'")
    }
  }

  #Loop over all the rows
  purrr::map(.x = 1:nrow(meta),
             .f = rowwise_extract)
}

