#' Retrieve Latitude and Longitude for a Given Location
#'
#' This function takes a location string, converts spaces to '+', and retrieves latitude and longitude coordinates for that location using the Nominatim API from OpenStreetMap.
#'
#' @param location A character string representing the location for which latitude and longitude are to be retrieved.
#' @return A list containing the latitude and longitude coordinates of the given location.
#' @examples
#' \dontrun{
#'   result <- lat_long("New York City")
#'   print(result)
#' }
#' @export
#' @import httr
#' @import rvest

lat_long<-function(loca_tion)
{
  loca_tion <- gsub(" ", "+", loca_tion) # replacing 'space' with '+' to get the location
  X<-list(address=loca_tion) 
  url_lnk <- "https://nominatim.openstreetmap.org/search?q="
  get_coords = paste0(url_lnk, loca_tion, "&format=geojson")
  
  response <- read_html(get_coords) %>%
    html_node("p") %>%
    html_text() %>%
    fromJSON()
  
  lat <- response$features$geometry$coordinates[[1]][2]
  lon <- response$features$geometry$coordinates[[1]][1]
  
  
  list("latitude"=lat, "longitude" =lon)
}
