#install.packages(c("leaflet", "dplyr", "htmlwidgets", "rnaturalearth", "rnaturalearthdata"))

setwd("~/Library/CloudStorage/GoogleDrive-l.haddock1510@gmail.com/My Drive/Acrobatic_Gymnastics/1_PUR_acro/PAGU_2025/PAGU2025_website")

library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmlwidgets)
library(sf)
library(htmltools)

st_shift <- function(x, x_shift = 0, y_shift = 0) {
  st_geometry(x) <- st_geometry(x) + c(x_shift, y_shift)
  return(x)
}

counts_named <- c(
  "United States" = 81,
  "Canada" = 57,
  "Mexico" = 21,
  "Puerto Rico" = 52,
  "Brazil" = 15,
  "Venezuela" = 0,
  "Argentina" = 6,
  "Colombia" = 5
)

counts_data <- data.frame(
  name_long = names(counts_named),
  count = as.numeric(counts_named),
  stringsAsFactors = FALSE
)

world <- ne_countries(scale = "medium", returnclass = "sf")

americas <- world %>%
  filter(region_un == "Americas", name_long != "Greenland")

americas_counts <- americas %>%
  left_join(counts_data, by = "name_long") %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Added 50–75 bin
bins_gt1 <- c(2, 5, 10, 25, 50, 75, 100, Inf)
pal_gt1 <- colorBin(
  palette = c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#084081", "#08306b"),
  domain  = americas_counts$count,
  bins    = bins_gt1,
  right   = TRUE
)

fill_cols <- ifelse(
  americas_counts$count <= 1,
  "#e9ecef",
  pal_gt1(americas_counts$count)
)

puerto_rico_bounds <- list(
  c(17.5, -67.5),
  c(18.7, -65.2)
)

map <- leaflet(americas_counts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = fill_cols,
    color     = ~ifelse(count > 1, "#2b2b2b", "#777777"),
    weight    = ~ifelse(count > 1, 1.5, 0.5),
    fillOpacity = ~ifelse(count > 1, 0.9, 0.6),
    highlightOptions = highlightOptions(weight = 2, color = "#000", fillOpacity = 1),
    label = lapply(
      paste0("<strong>", americas_counts$name_long, "</strong><br>Registered Athletes: ", americas_counts$count),
      htmltools::HTML
    ),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "#e9ecef",
    labels = "0–1",
    title = "Athletes",
    opacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_gt1,
    values = ~count,
    labFormat = labelFormat(digits = 0, transform = function(x) x),
    opacity = 1
  ) %>%
  addRectangles(
    lng1 = puerto_rico_bounds[[1]][2], lat1 = puerto_rico_bounds[[1]][1],
    lng2 = puerto_rico_bounds[[2]][2], lat2 = puerto_rico_bounds[[2]][1],
    fill = FALSE,
    color = "red",
    weight = 2,
    popup = "Zoom in: Puerto Rico"
  )

print(map)

saveWidget(map, "americas_full_map.html", selfcontained = TRUE)
