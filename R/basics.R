library(jsonlite)
library(httr)

url <- "https://data.bus-data.dft.gov.uk/api/v1/dataset?limit=25&search=Stagecoach&status=published&api_key=KEY HERE"

download <- httr::GET(url)


##Return err message if one or more atco codes are invalid
if(http_status(download)$category != "Success"){
  stop("An atco area code provided does not exist. Please check your query.
    To display all valid atco codes, run lookup_atco_codes()")
}

data <- fromJSON(content(download, as = "text", encoding = "UTF-8"))$results
