library(readxl)
library(tidyverse)
library(magrittr)
library(rmapshaper)

# Shapefiles --------------------------------------------------------------

# Federal Electoral Districts shapefile

shape_file_url <- "http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/Elections_Canada_2019/federal_electoral_districts_boundaries_2019.shp.zip"
download.file(shape_file_url,
              destfile = "data/federal_electoral_districts_boundaries_2019_shp_en.zip", quiet = TRUE)
unzip("data/federal_electoral_districts_boundaries_2019_shp_en.zip", exdir="data/federal_electoral_districts_boundaries_2019_shp_en")
federal_shapefile <- sf::read_sf("data/federal_electoral_districts_boundaries_2019_shp_en",
                                 layer = "FED_CA_2019_EN") %>%
  sf::st_transform(crs = "+init=epsg:4326") %>%
  rmapshaper::ms_simplify()

# Toronto wards

download.file("http://opendata.toronto.ca/gcc/voting_location_2018_wgs84.zip",
              destfile = "data/voting_location_2018_wgs84.zip", quiet = TRUE)
unzip("data/voting_location_2018_wgs84.zip", exdir="data/voting_location_2018_wgs84")
  
toronto_wards <- sf::read_sf("data/voting_location_2018_wgs84", layer = "VOTING_LOCATION_2018_WGS84") %>%
  sf::st_transform(crs = "+init=epsg:4326")



# VPI data ----------------------------------------------------------------

vpi_raw <- read_excel("data/VPI.xlsx", col_types = c("numeric", "numeric", "numeric"))


# Combine -----------------------------------------------------------------

vpi_geo <- dplyr::left_join(federal_shapefile, vpi_raw, by = c("FEDNUM" = "ED")) %>%
  dplyr::rename(electoral_district = FEDNUM)

vpi_geo_to <- vpi_geo[toronto_wards,]

# Plot --------------------------------------------------------------------

ggplot2::ggplot(data = vpi_geo) +
  ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
  ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 6), "High")) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

ggplot2::ggplot(data = dplyr::filter(vpi_geo, dplyr::between(electoral_district, 35000, 36000))) +
  ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
  ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 6), "High")) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

ggplot2::ggplot(data = dplyr::filter(vpi_geo, dplyr::between(electoral_district, 48000, 49000))) +
  ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
  ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 6), "High")) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

ggplot2::ggplot(data = dplyr::filter(vpi_geo, dplyr::between(electoral_district, 59000, 60000))) +
  ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
  ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 6), "High")) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

ggplot2::ggplot(data = vpi_geo_to) +
  ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
  ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 6), "High")) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())

# ggplot2::geom_sf(ggplot2::aes(fill = ggplot2::cut_interval(VPI, 9))) +
#   ggplot2::scale_fill_brewer("VPI", palette = "YlOrBr", labels=c("Low", rep("", 7), "High") +

