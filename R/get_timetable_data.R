urls <- bodsr::get_timetable_metadata()

urls




library(httr)


##Try to unzip with names
if(urls[4, "extension"] == "zip"){

  zip_list(urls[4, "url"])

  library(xml2)

  ##Download and read xml
  folder <- tempfile(fileext = ".xml")

  httr::GET(
    url = url,
    write_disk(folder, overwrite = TRUE)
  )

  xml <- xml2::read_xml(folder)


  ##Get operator name
  find_node_value(xml, "//d1:TradingName")

  find_node_value(xml, "//d1:OperatorCode")



    xml_find_all("./LicensedOperator")

  a <- xml_find_all(xml, "//.S*")

  doc <- XML::xmlParse(files[1])
  list <- xmlToList(doc)

  getNodeSet(doc, "//n:OperatingHours", namespaces)


  }
##Each file = 1 df

## Make a list of dfs
