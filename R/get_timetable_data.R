file <- bodsr::get_timetable_metadata()[4,]

extract_zip_or_xml <- function(file){

  ##Try to unzip with names if it's a zip
  if(file$extension == "zip"){

    open_all_xml(file$url)

  } else if(file$extension == "xml"){

    ##Download and open xml file
    xml_loc <- tempfile(fileext = ".xml")

    httr::GET(
      url = file$url,
      write_disk(xml_loc, overwrite = TRUE)
    )

    line_level_xml(xml_loc)

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
      extract_zip_or_xml(meta) %>%
      ##Join to the URL deets
      bind_cols(meta)
}

extract_single_line(file)
