##Gather individual stop-level timing data from the journey data
build_stops <- function(j){

  ##node name with number in it
  node_name <- paste0("//d1:JourneyPatternSections//d1:JourneyPatternSection[",
                      j, "]//d1:JourneyPatternTimingLink")

  inner_stops <- function(i, node_name){

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

##Gather vehicle code and departure time lookup table
build_vehicle_pattern <- function(i){
  tibble::tibble(
      "LineRef" = find_node_value(xml, "//d1:LineRef"),
      "VehicleJourneyCode" = find_node_value(xml, "//d1:VehicleJourneyCode"),
      "JourneyPatternRef" = find_node_value(xml, "//d1:JourneyPatternRef"),
      "DepartureTime" = find_node_value(xml, "//d1:DepartureTime")
    )
}


##Gather individual stop-level timing data from the vehicle data
build_vehicle_timing_link <- function(i){

  ##node name with number in it
  node_name <- paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
         "]/d1:VehicleJourneyTimingLink")

  ##Pull out the vehicle timing ref and runtime for each link
  link_ref_runtime <- function(j, node_name){
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


##Create service journey and section reference lookup
unwrap_service_xml <- function(i){

  tibble::tibble(
    "JourneyPatternRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                      "]/@id")),
    "JourneyPatternSectionRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                             "]/d1:JourneyPatternSectionRefs"))
  )
}



##Get stop-level data from xml
stop_level_xml <- function(xml, count, total_count){

  message("Reading file", count, "of", total_count)

  xml <- xml2::read_xml(xml)

  #Extract all 4 data sets
  ##Runtime from journey data
  times_j <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:JourneyPatternSections//d1:JourneyPatternSection"),
                      .f = build_stops)

  #Journey pattern and section lookups
  jps_lookup <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:StandardService/d1:JourneyPattern"),
                       .f = unwrap_service_xml)

  #Vehicle journey codes and times
  vcodes <- build_vehicle_pattern()

  ##Journey times from vehicle data
  times_v <-  purrr::map_df(.x = 1:count_nodes(xml, "//d1:VehicleJourneys/d1:VehicleJourney"),
                        .f = build_vehicle_timing_link) %>%
    unique()

  ##Join everything up together
  ##Join journey runtimes onto journey and vehicle codes
  dplyr::left_join(times_j, jps_lookup, by = "JourneyPatternSectionRef") %>%
    dplyr::left_join(
      #Join vehicle journey times and patterns, then joint them to runtimes
      dplyr::left_join(vcodes, times_v, by = c("LineRef", "JourneyPatternRef")),
      by = c("jp_LinkRef", "JourneyPatternRef")) %>%
    #Keep the cols we care about
    dplyr::select(LineRef, SequenceNumber, VehicleJourneyCode, StopFrom, StopTo, DepartureTime, RunTime_journey, RunTime_vehicle)
}

