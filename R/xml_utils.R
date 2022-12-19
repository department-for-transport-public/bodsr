##Utility functions for working with XML

##Return a specific value from xml
find_node_value <- function(x, xpath){

  xml2::xml_find_all(x = x, xpath = xpath) %>%
    xml2::as_list() %>%
    unlist()
}

##Get line-level details from an xml file
line_level_xml <- function(x){

  xml <- xml2::read_xml(x)
  ##Create a table of values
  tibble::tibble(
    ##Get operator name
    "tradingName" = find_node_value(xml, "//d1:TradingName"),
    #Operator code
    "operatorCode" = find_node_value(xml, "//d1:OperatorCode"),
    #Licence number
    "licenceNumber" = find_node_value(xml, "//d1:LicenceNumber[1]"),
    #Service code
    "serviceCode" = paste(licenceNumber, find_node_value(xml, "//d1:ServiceCode"), sep = ":"),
    ##Line names
    "lineName" = find_node_value(xml, "//d1:LineName"))
}

##Open every XML file in a zip and link them up to the names
open_all_xml <- function(url){

  ##Download to temp location
  zip_loc <- tempfile()
  folder <- tempdir()

  httr::GET(
    url = url,
    write_disk(zip_loc, overwrite = TRUE)
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
