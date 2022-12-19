##Utility functions for working with XML

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

##Download and read xml
get_raw_xml <- function(url){

  xml_loc <- tempfile(fileext = ".xml")

  httr::GET(
    url = url,
    write_disk(xml_loc, overwrite = TRUE)
  )

  xml2::read_xml(xml_loc)

}

##Return a specific value from xml
find_node_value <- function(x, xpath){

  xml2::xml_find_all(x = x, xpath = xpath) %>%
    xml2::as_list() %>%
    unlist()
}

##Get line-level details from an xml file
line_level_xml <- function(xml){

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
