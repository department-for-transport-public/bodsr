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

##Map this over every JPS value
df <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:JourneyPatternSections//d1:JourneyPatternSection"),
              .f = build_stops)


build_journey_pattern <- function(i){
  tibble::tibble(
      "LineRef" = find_node_value(xml, "//d1:LineRef"),
      "VehicleJourneyCode" = find_node_value(xml, "//d1:VehicleJourneyCode"),
      "JourneyPatternRef" = find_node_value(xml, "//d1:JourneyPatternRef"),
      "DepartureTime" = find_node_value(xml, "//d1:DepartureTime")
    )
}


df1 <- build_journey_pattern()

##Create vehicle timing for each vehicle journey
build_vehicle_timing_link <- function(i){

  ##Pull out the vehicle timing ref and runtime for each link
  link_ref_runtime <- function(j){
    tibble::tibble(
      "jpt_LinkRef" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                                "]/d1:VehicleJourneyTimingLink[", j,
                                                "]/d1:JourneyPatternTimingLinkRef")),
      "RunTime_vehicle" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                            "]/d1:VehicleJourneyTimingLink[", j, "]/d1:RunTime"))
    )
  }

  ##Loop over every link
  purrr::map_df(.x = 1:count_nodes(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                               "]/d1:VehicleJourneyTimingLink")),
                .f = link_ref_runtime) %>%
    dplyr::bind_cols(


      tibble::tibble(
        "JourneyPatternRef" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                                        "]/d1:JourneyPatternRef")),
        "LineRef" = find_node_value(xml, paste0("//d1:VehicleJourneys/d1:VehicleJourney[", i,
                                              "]/d1:LineRef"))
      )
    )
}

##For each vehicle journey, produce link deets
df2 <-  purrr::map_df(.x = 1:count_nodes(xml, "//d1:VehicleJourneys/d1:VehicleJourney"),
              .f = build_vehicle_timing_link) %>%
  unique()


##Create service journey and section reference lookup
unwrap_service_xml <- function(i){

  tibble::tibble(
    "JourneyPatternRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                      "]/@id")),
    "JourneyPatternSectionRef" = find_node_value(xml, paste0("//d1:StandardService/d1:JourneyPattern[", i,
                                                             "]/d1:JourneyPatternSectionRefs"))
  )
}


df3 <- purrr::map_df(.x = 1:count_nodes(xml, "//d1:StandardService/d1:JourneyPattern"),
              .f = unwrap_service_xml)



##Join everything up together
##Join journey runtimes onto journey and vehicle codes
dplyr::left_join(df, df3, by = "JourneyPatternSectionRef") %>%
  dplyr::left_join(df1, by = c("LineRef", "JourneyPatternRef")) %>%
  dplyr::left_join(df2) %>%
  #Keep the cols we care about
  dplyr::select(LineRef, SequenceNumber, VehicleJourneyCode, StopFrom, StopTo, DepartureTime, RunTime_journey, RunTime_vehicle) %>%
  #Keep only unique values
  unique() %>%
  View()


