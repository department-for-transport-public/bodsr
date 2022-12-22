##Utility functions for working with XML

#' @name find_node_value
#' @title Search an xml file for a specific named mode and return the value(s) stored in it
#'
#' @param x An xml object
#' @param xpath string. The node name to search for within the xpath.
#'
#' @importFrom xml2 xml_find_all as_list xml_ns
#'
#' @return Returns a single string of values from the specified node

##Return a specific value from xml
find_node_value <- function(x, xpath){

  ##If the d1 namespace is missing, return NA
  if(any(grepl("d1", names(xml2::xml_ns(x))))){
    xml2::xml_find_all(x = x, xpath = xpath) %>%
      xml2::as_list() %>%
      unlist()
    } else{
        NULL
      }

}

#' @name line_level_xml
#'
#' @param x An xml object
#' @title Pull a table of relevant values from specified nodes in the xml
#'
#' @importFrom xml2 read_xml
#' @importFrom tibble tibble
#' @importFrom purrr possibly
#'
#' @return Returns a table of values extracted from specified nodes of
#' an xml document

##Get line-level details from an xml file
line_level_xml <- function(x){

  ##Create safe version of function that quietly fails
  poss_xml <- purrr::possibly(xml2::read_xml, otherwise = NULL)

  xml <- poss_xml(x)

  if(!is.null(xml)){
  ##Create a table of values
      values <- tibble::tibble(
        ##Get operator name
        "tradingName" = find_node_value(xml, "//d1:TradingName"),
        #Operator code
        "operatorCode" = find_node_value(xml, "//d1:OperatorCode"),
        #Licence number
        "licenceNumber" = find_node_value(xml, "//d1:LicenceNumber[1]"),
        #Service code
        "serviceCode" = paste(find_node_value(xml, "//d1:LicenceNumber[1]"),
                              find_node_value(xml, "//d1:ServiceCode"), sep = ":"),
        ##Line names
        "lineName" = find_node_value(xml, "//d1:LineName"))
      } else{

        #Create a blank table of values
        values <- tibble::tibble(
          ##Get operator name
          "tradingName" = character(),
          #Operator code
          "operatorCode" = character(),
          #Licence number
          "licenceNumber" = character(),
          #Service code
          "serviceCode" = character(),
          ##Line names
          "lineName" = character())
      }

    #If our tibble is blank, give a warning
  if(nrow(values) == 0){
    warning("File could not be read in:", x)
    }

  return(values)
  }

#' @name open_all_xml
#' @title Open every xml file within a zip object and extract data of interest from it
#'
#' @param url A url pointing towards a zip object
#'
#' @importFrom utils unzip
#' @importFrom httr write_disk GET
#' @importFrom purrr map_df
#'
#'
#' @return returns a dataframe of information extracted from xml documents

##Open every XML file in a zip and link them up to the names
open_all_xml <- function(url){

  ##Download to temp location
  zip_loc <- tempfile()
  folder <- tempdir()

  httr::GET(
    url = url,
    httr::write_disk(zip_loc, overwrite = TRUE)
  )

  ##Unzip the zip file to the temp location
  utils::unzip(zip_loc, exdir = folder)

  ##Files to read in
  files_to_read <- list.files(folder, full.names = TRUE, pattern = "\\.xml")

  ##For each item in the folder, run extracting the single line over it
  purrr::map_df(.x = files_to_read,
                .f = line_level_xml,
                .id = "filepath")
}

