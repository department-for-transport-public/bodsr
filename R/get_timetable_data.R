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

meta <- bodsr::get_timetable_metadata()[1,]

extract_zip_or_xml <- function(file){

  ##Try to unzip with names if it's a zip
  if(file$extension == "zip"){

    zip_list(file$url)

  } else if(file$extension == "xml"){

    extract_single_line(file)

  }else{
    stop("Unsupported file type")
  }

}

##Extract data from xml file for a single line
extract_single_line <- function(x){

  ##Extract metadata that applies to all files
  meta <- x %>%
    dplyr::select(url, dataSetID = id,
                  operatorName, description, status, extension, dqScore, dqRag)

    ##Read in the xml
    get_raw_xml(meta$url) %>%
    ##Get line-level detail for it
      line_level_xml() %>%
      ##Join to the URL deets
      bind_cols(meta)
}

extract_zip_or_xml(meta)
