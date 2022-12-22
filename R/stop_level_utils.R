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


xml %>%
  xml2::xml_find_all("//d1:LineRef")

##Gather individual stop-level data
build_stops <- function(i){
  tibble::tibble(
    "jptl_id" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/@id")),
    "RunTime" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:RunTime")),
    "StopFrom" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/d1:StopPointRef")),
    "StopTo" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:To/d1:StopPointRef")),
    "SequenceNumber" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/d1:SequenceNumber")),
    "TimingStatus" = find_node_value(xml, paste0("//d1:JourneyPatternTimingLink[", i, "]/d1:From/d1:TimingStatus"))
                 )
}

purrr::map_df(.x = 1:length(list1),
              .f = build_stops)


