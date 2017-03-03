install.packages("leaflet")
install.packages("geojsonio")
install.packages("WDI")
install.packages("sp")
devtools::install_github("rstudio/leaflet")
library(leaflet)
library(sp)
library(geojsonio)
states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)
states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
class(states)

library(rgdal)
library(WDI)
library(leaflet)

### Function to create a Leaflet interactive map in RStudio from a World Bank indicator.

wdi_leaflet <- function(indicator, indicator_alias = "Value", year = 2012, classes = 5, colors = "Blues") {

  url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

  tmp <- tempdir()

  file <- basename(url)

  download.file(url, file)

  unzip(file, exdir = tmp)

  countries <- readOGR(dsn = tmp,
                       layer = "ne_50m_admin_0_countries",
                       encoding = "UTF-8",
                       verbose = FALSE)


  dat <- WDI(country = "all",
             indicator = indicator,
             start = year,
             end = year)

  dat[[indicator]] <- round(dat[[indicator]], 1)



  countries2 <- merge(countries,
                      dat,
                      by.x = "iso_a2",
                      by.y = "iso2c",
                      sort = FALSE)

  pal <- colorQuantile(colors, NULL, n = classes)

  country_popup <- paste0("<strong>Country: </strong>",
                          countries2$country,
                          "<br><strong>",
                          indicator_alias,
                          ", ",
                          as.character(year),
                          ": </strong>",
                          countries2[[indicator]])

  stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"

  stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

  leaflet(data = countries2) %>%
    addTiles(urlTemplate = stamen_tiles,
             attribution = stamen_attribution) %>%
    setView(0, 0, zoom = 3) %>%
    addPolygons(fillColor = ~pal(countries2[[indicator]]),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                popup = country_popup)

}

## Example call

wdi_leaflet(indicator = "SP.RUR.TOTL.ZS", indicator_alias = "Percent rural", colors = "OrRd")


####

indicator = "SP.RUR.TOTL.ZS"
indicator_alias = "Percent rural"
colors = "OrRd"
year = 2012
classes = 5

  url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

  tmp <- tempdir()

  file <- basename(url)

  download.file(url, file)

  unzip(file, exdir = tmp)

  countries <- readOGR(dsn = tmp,
                       layer = "ne_50m_admin_0_countries",
                       encoding = "UTF-8",
                       verbose = FALSE)


  dat <- WDI(country = "all",
             indicator = indicator,
             start = year,
             end = year)

  dat[[indicator]] <- round(dat[[indicator]], 1)



  countries2 <- merge(countries,
                      dat,
                      by.x = "iso_a2",
                      by.y = "iso2c",
                      sort = FALSE)

  pal <- colorQuantile(colors, NULL, n = classes)

  country_popup <- paste0("<strong>Country: </strong>",
                          countries2$country,
                          "<br><strong>",
                          indicator_alias,
                          ", ",
                          as.character(year),
                          ": </strong>",
                          countries2[[indicator]])

  stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"

  stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

  leaflet(data = countries2) %>%
    addTiles(urlTemplate = stamen_tiles,
             attribution = stamen_attribution) %>%
    setView(0, 0, zoom = 3) %>%
    addPolygons(fillColor = ~pal(countries2[[indicator]]),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                popup = country_popup)

  leaflet(data = dat) %>%
    addTiles() %>%
    setView(0, 0, zoom = 3) %>%
    addPolygons()


  states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
