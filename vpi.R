library(readxl)
library(tidyverse)
library(magrittr)

# Shapefiles --------------------------------------------------------------

# Federal Electoral Districts shapefile

shape_file_url <- "http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/Elections_Canada_2019/federal_electoral_districts_boundaries_2019.shp.zip"
download.file(shape_file_url,
              destfile = "data/federal_electoral_districts_boundaries_2019_shp_en.zip", quiet = TRUE)
unzip("data/federal_electoral_districts_boundaries_2019_shp_en.zip", exdir="data/federal_electoral_districts_boundaries_2019_shp_en")
federal_shapefile <- sf::read_sf("data/federal_electoral_districts_boundaries_2019_shp_en",
                                 layer = "FED_CA_2019_EN") %>%
  sf::st_transform(crs = "+init=epsg:4326")

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

vpi_geo$VPI[vpi_geo$VPI>2] <- 1.5

vpi_geo_to <- vpi_geo[toronto_wards,]

# Plot --------------------------------------------------------------------

ed_zoom_map <- function(low = 1, high = 70000, filename = "ca_vpi") {
  p <- ggplot2::ggplot(data = dplyr::filter(vpi_geo, dplyr::between(electoral_district, low, high))) +
    ggplot2::geom_sf(ggplot2::aes(fill = VPI)) +
    ggplot2::scale_fill_fermenter("VPI", type = "div", palette = "BrBG") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  ggplot2::ggsave(paste0(filename, ".png"))
  p
}

ed_zoom_map()
ed_zoom_map(high = 60000, filename = "south_ca_vpi")
ed_zoom_map(low = 35000, high = 36000, filename = "on_vpi")
ed_zoom_map(low = 37000, high = 49000, filename = "midwest_vpi")
ed_zoom_map(low = 59000, high = 60000, filename = "bc_vpi")
ed_zoom_map(low = 24000, high = 25000, filename = "qc_vpi")
ed_zoom_map(high = 23000, filename = "at_vpi")

bbox_zoom_map <- function(xlim = c(-141.0181, -52.5823), 
                          ylim = c(41.67695, 89.99943),
                          filename = "ca_vpi") { # TODO: Switch these to defaults, unless specified
  p <- ggplot2::ggplot(data = vpi_geo) +
    ggplot2::geom_sf(ggplot2::aes(fill = VPI)) +
    coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::scale_fill_fermenter("VPI", type = "div", palette = "BrBG") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
  ggplot2::ggsave(paste0(filename, "_zoom.png"))
  p
}

bbox_zoom_map(xlim = c(-82, -76), ylim = c(42, 46), filename = "ggh_vpi") # GGH
bbox_zoom_map(xlim = c(-119, -111), ylim = c(48, 55), filename = "ab_vpi") #AB
bbox_zoom_map(xlim = c(-129, -121), ylim = c(48, 51.5), filename = "bc_vpi") #BC
bbox_zoom_map(xlim = c(-76, -70), ylim = c(44.5, 47), filename = "qc_vpi") #QC
bbox_zoom_map(xlim = c(-75, -53), ylim = c(43, 53), filename = "at_vpi") #AT
bbox_zoom_map(xlim = c(-74, -73), ylim = c(45, 46), filename = "mt_vpi") #MT

# Toronto districts
ggplot2::ggplot(data = vpi_geo_to) +
  ggplot2::geom_sf(ggplot2::aes(fill = VPI)) +
  ggplot2::scale_fill_fermenter("VPI", type = "div", palette = "BrBG") +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank()) +
  ggplot2::ggsave("to_vpi.png")
