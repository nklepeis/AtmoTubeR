#' @name plot_AT_map_leaflet
#'
#' @title Plot AtmoTube trajectory on a interactive map using leaflet
#'
#' @param data a tibble of AtmoTube data from Cloud API
#'
#' @author Neil E. Klepeis
#'
#' @details This function plots an interactive map showing where the
#'  AtmoTube was located by day/hour/etc
#'
# ----------------------------------------------------------------

plot_AT_map_leaflet <- function(data) {

  library(leaflet)
  library(lubridate)
  library(dplyr)

  data <- data %>%
    select(`Date (GMT)`, starts_with(c("Latitude","Longitude"))) %>%
    rename(Time = `Date (GMT)`) %>%
    arrange(Time) %>%
    mutate(Time = format(with_tz(Time, "America/Los_Angeles"),"%m/%d/%y %I:%M %p")) %>%
    distinct(Latitude, Longitude, .keep_all=TRUE)
  print(data)

  m <- leaflet(
    padding=8,
    options=leafletOptions(zoomControl=TRUE)
  ) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=data$Longitude, lat=data$Latitude,
               popup=paste0("AtmoTube", "<br>", data$Longitude,", ",
                            data$Latitude,"<hr>", data$Time),
               group="Monitors") %>%
    addPolylines(lng=data$Longitude, lat=data$Latitude,
                 color="red") %>%
    addProviderTiles('Esri.WorldImagery', group="Imagery") %>%
    #addProviderTiles("Stamen.TerrainBackground", group="Terrain") %>%
    addProviderTiles("Esri.WorldTopoMap", group="Terrain") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels", group="Labels") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group="Streets") %>%
    addProviderTiles(providers$Stamen.Toner, group="Toner") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Positron") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group="NatGeo") %>%
    addMeasure(position="topleft",
               activeColor="red",
               completedColor="black") %>%
    addLayersControl(baseGroups = c("Streets","Imagery", "Terrain","Toner","Positron","NatGeo"),
                     overlayGroups = c("Monitors","Labels")
    )

  m

}
