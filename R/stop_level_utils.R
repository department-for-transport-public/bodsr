##Get stop-level data
urls <- get_timetable_metadata(status = "published")

file <- urls[4,]

##Download and open xml file
xml_loc <- tempfile(fileext = ".xml")

httr::GET(
  url = file$url,
  httr::write_disk(xml_loc, overwrite = TRUE)
)


xml <- xml2::read_xml(xml_loc)


##Gather individual stop-level data from the timing data
build_stops <- function(i){
  tibble::tibble(
    "jptl_id" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/@id")),
    "RunTime" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:RunTime")),
    "StopFrom" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/d1:StopPointRef")),
    "StopTo" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:To/d1:StopPointRef")),
    "SequenceNumber" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/@SequenceNumber")),
    "TimingStatus" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/d1:TimingStatus"))
                 )
}


purrr::map_df(.x = 1:count_nodes(xml, "//d1:JourneyPatternTimingLink"),
              .f = build_stops)

build_journey_pattern <- function(i){
  tibble::tibble(
    "LineRef" = find_node_value(xml, "//d1:LineRef"),
    "VehicleJourneyCode" = find_node_value(xml, "//d1:VehicleJourneyCode"),
    "JourneyPatternRef" = find_node_value(xml, "//d1:JourneyPatternRef"),
    "DepartureTime" = find_node_value(xml, "//d1:DepartureTime")
  )
}


##Create vehicle timing for each vehicle journey
build_vehicle_timing_link <- function(i){

  ##Pull out the vehicle timing ref and runtime for each link
  link_ref_runtime <- function(i, j){
    tibble::tibble(
      jpt_LinkRef = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                                "]/d1:VehicleJourneyTimingLink[", j,
                                                "]/d1:JourneyPatternTimingLinkRef")),
      runTime = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                            "]/d1:VehicleJourneyTimingLink[", j, "]/d1:RunTime"))
    )
  }

  ##Loop over every link
  purrr::map_df(.x = 1:count_nodes(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                               "]/d1:VehicleJourneyTimingLink")),
                .f = link_ref_runtime,
                i = i) %>%
    dplyr::bind_cols(


      tibble::tibble(
        JourneyPatternRef = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                                        "]/d1:JourneyPatternRef")),
        LineRef = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                              "]/d1:LineRef"))
      )
    )
}

##For each vehicle journey, produce link deets
purrr::map_df(.x = 1:count_nodes(xml, "//d1:VehicleJourneys/d1:VehicleJourney"),
              .f = build_vehicle_timing_link)






