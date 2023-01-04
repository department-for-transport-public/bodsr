#' @name extract_stop_timing
#' @title Extract stop-level timing data from journey data in the provided xml document
#'
#' @param j A vector of node numbers to loop over
#' @param xml an xml object containing the relevant nodes
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom dplyr mutate "%>%"
#'
#' @return returns a dataframe of information extracted from the given nodes in the xml

##Gather individual stop-level timing data from the journey data
extract_stop_timing <- function(j, xml){

  ##node name with number in it
  node_name <- paste0("//d1:JourneyPatternSections//d1:JourneyPatternSection[",
                      j, "]//d1:JourneyPatternTimingLink")

  inner_stops <- function(i, node_name, xml = xml){

    tibble::tibble(
      "jp_LinkRef" = find_node_value(xml, paste0(node_name, "[", i, "]/@id")),
      "RunTime_journey" = find_node_value(xml, paste0(node_name, "[", i, "]/d1:RunTime")),
      "StopFrom" = find_node_value(xml, paste0(node_name, "[", i, "]/d1:From/d1:StopPointRef")),
      "StopTo" = find_node_value(xml, paste0(node_name, "[", i, "]/d1:To/d1:StopPointRef")),
      "SequenceNumber" = find_node_value(xml, paste0(node_name, "[", i, "]/d1:From/@SequenceNumber")),
      "TimingStatus" = find_node_value(xml,paste0(node_name, "[", i, "]/d1:From/d1:TimingStatus"))
      )
  }

  #Map this over every individual id
    purrr::map_df(.x = 1:count_nodes(xml, node_name),
                  .f = inner_stops,
                  node_name = node_name) %>%
      dplyr::mutate("JourneyPatternSectionRef" = find_node_value(xml, paste0("//d1:JourneyPatternSections//d1:JourneyPatternSection[", j, "]/@id")))

}

#' @name extract_vehicle_journeys
#' @title Extract vehicle-level journey departure times
#' @param xml an xml object containing the relevant nodes
#'
#' @importFrom tibble tibble
#'
#' @return returns a dataframe of information extracted from the given nodes in the xml

extract_vehicle_journeys <- function(xml){
  tibble::tibble(
      "LineRef" = find_node_value(xml, "//d1:LineRef"),
      "VehicleJourneyCode" = find_node_value(xml, "//d1:VehicleJourneyCode"),
      "JourneyPatternRef" = find_node_value(xml, "//d1:JourneyPatternRef"),
      "DepartureTime" = find_node_value(xml, "//d1:DepartureTime")
    )
}


#' @name extract_vehicle_timing
#' @title Extract stop-level timing data from vehicle data in the provided xml document
#'
#' @param i A vector of node numbers to loop over
#' @param xml an xml object containing the relevant nodes
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom dplyr bind_cols
#'
#' @return returns a dataframe of information extracted from the given nodes in the xml

extract_vehicle_timing <- function(i, xml){

  ##node name with number in it
  node_name <- paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
         "]/d1:VehicleJourneyTimingLink")

  ##Pull out the vehicle timing ref and runtime for each link
  link_ref_runtime <- function(j, node_name, xml = xml){
    tibble::tibble(
      "jp_LinkRef" = find_node_value(xml, paste0(node_name, "[", j,
                                                "]/d1:JourneyPatternTimingLinkRef")),
      "RunTime_vehicle" = find_node_value(xml, paste0(node_name, "[", j, "]/d1:RunTime"))
    )
  }

  ##Loop over every link
  purrr::map_df(.x = 1:count_nodes(xml, node_name),
                .f = link_ref_runtime,
                node_name = node_name) %>%
    dplyr::bind_cols(


      tibble::tibble(
        "JourneyPatternRef" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                                        "]/d1:JourneyPatternRef")),
        "LineRef" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                              "]/d1:LineRef"))
      )
    )
}


#' @name extract_service_lookup
#' @title Extract service journey and section reference lookup from the provided xml document
#'
#' @param i A vector of node numbers to loop over
#' @param xml an xml object containing the relevant nodes
#'
#' @importFrom tibble tibble
#'
#' @return returns a dataframe of information extracted from the given nodes in the xml

extract_service_lookup <- function(i, xml){

  tibble::tibble(
    "JourneyPatternRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                      "]/@id")),
    "JourneyPatternSectionRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                             "]/d1:JourneyPatternSectionRefs"))
  )
}



#' @name stop_level_xml
#' @title Extract a tidy dataset of stop-level journey data from the provided xml document
#'
#' @param xml an xml object containing the relevant nodes
#' @param count numeric. Where the xml file is taken from a zip collection, the number file it is. Defaults to 1.
#' @param total_count numeric. Where the xml file is taken from a zip collection, the total number of files in the zip. Defaults to 1.
#'
#' @importFrom xml2 read_xml
#' @importFrom purrr map_df
#' @importFrom dplyr select left_join "%>%"
#'
#' @return returns a dataframe of information extracted from the given nodes in the xml

##Get stop-level data from xml
stop_level_xml <- function(xml, count = 1, total_count = 1){

  #Return progress message
  message("Reading file ", count, " of ", total_count)

  xml <- xml2::read_xml(xml)

  #Extract all 4 data sets
  ##Runtime from journey data
  times_j <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:JourneyPatternSections//d1:JourneyPatternSection"),
                      .f = extract_stop_timing,
                      xml = xml)

  #Journey pattern and section lookups
  jps_lookup <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:StandardService/d1:JourneyPattern"),
                       .f = extract_service_lookup,
                       xml = xml)

  #Vehicle journey codes and times
  vcodes <- extract_vehicle_journeys(xml)

  ##Journey times from vehicle data
  times_v <-  purrr::map_df(.x = 1:count_nodes(xml, "//d1:VehicleJourneys/d1:VehicleJourney"),
                        .f = extract_vehicle_timing,
                        xml = xml) %>%
    unique()

  ##Join everything up together
  ##Join journey runtimes onto journey and vehicle codes
  dplyr::left_join(times_j, jps_lookup, by = "JourneyPatternSectionRef") %>%
    dplyr::left_join(
      #Join vehicle journey times and patterns, then joint them to runtimes
      dplyr::left_join(vcodes, times_v, by = c("LineRef", "JourneyPatternRef")),
      by = c("jp_LinkRef", "JourneyPatternRef")) %>%
    #Keep the cols we care about
    dplyr::select(LineRef, SequenceNumber, VehicleJourneyCode, StopFrom, StopTo,
                  DepartureTime, RunTime_journey, RunTime_vehicle)
}


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
