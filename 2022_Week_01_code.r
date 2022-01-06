# Loading libraries
library(tidyverse)
library(sf)
library(osmdata)
library(showtext)
library(ggtext)


# Loading font
font_add_google("Ubuntu", "Ubuntu")
showtext_auto()


# Downloading OSM data
opq(bbox = "Sault Ste. Marie") %>% 
  add_osm_feature(key = 'highway', value = list("motorway", "trunk", "primary", "secondary", "tertiary")) %>% 
  osmdata_xml(filename = "SSM_highway.osm")

opq(bbox = "Sault Ste. Marie") %>% 
  add_osm_feature(key = 'footway') %>% 
  osmdata_xml(filename = "SSM_footway.osm")

opq(bbox = "Sault Ste. Marie") %>% 
  add_osm_feature(key = 'railway', value = "rail") %>% 
  osmdata_xml(filename = "SSM_railway.osm")

opq(bbox = "Sault Ste. Marie") %>% 
  add_osm_feature(key = 'water') %>%
  osmdata_xml(filename = "SSM_water.osm")

opq(bbox = "Sault Ste. Marie") %>% 
  add_osm_feature(key = 'landuse', value = list("commercial", "residential", "retail")) %>%
  osmdata_xml(filename = "SSM_landuse.osm")


# Loading layers
st_read("SSM_highway.osm", layer = "lines") %>% 
  mutate(legend = "Road*") -> SSM_highway

st_read("SSM_footway.osm", layer = "lines") %>% 
  mutate(legend = "Footway") -> SSM_footway

st_read("SSM_railway.osm", layer = "lines") %>% 
  mutate(legend = "Railway") -> SSM_railway

st_read("SSM_water.osm", layer = "multipolygons") -> SSM_water

st_read("SSM_landuse.osm", layer = "multipolygons") %>% 
  filter(landuse %in% c("commercial", "residential", "retail")) -> SSM_landuse

# Combining lines
SSM_lines <- bind_rows(SSM_highway, SSM_footway, SSM_railway) %>% 
  mutate(legend = factor(legend, levels = c("Road*", "Footway", "Railway")))


# Projecting layers to EPSG:2022
st_transform(SSM_lines, crs = 2022) -> SSM_lines2
st_transform(SSM_water, crs = 2022) -> SSM_water2
st_transform(SSM_landuse, crs = 2022) -> SSM_landuse2


# Plotting
result_plot <- ggplot() + 
  # landuse layer
  geom_sf(data = SSM_landuse2, aes(fill = landuse)) + 
  scale_fill_manual(labels = c("Commercial", "Residential", "Retail"), values = c("#19448e", "#44617b", "#165e83")) + 
  # lines layer
  geom_sf(data = SSM_lines2, aes(color = legend, size = legend), key_glyph = "rect") + 
  scale_color_manual(values = c("#e45e32", "#f39800", "#2b2b2b")) + 
  scale_size_manual(values = c(3.5, 1.5, 2.5)) + 
  # water layer
  geom_sf(data = SSM_water2, fill = "#165e83") + 
  # scale limits
  coord_sf(xlim = st_bbox(SSM_landuse2)[c(1, 3)], ylim = st_bbox(SSM_landuse2)[c(2, 4)]) + 
  # labs
  labs(
    title = "<b>Rough map</b> of <b><span style='color:#1e50a2'>Sault Ste. Marie</span></b>", 
    subtitle = "projected in <span style='color:#426579'>EPSG:2022</span>", 
    caption = "*'Road' contains 'motorway', 'trunk', 'primary', 'secondary', and 'tertiary'<br><br><b>Data from © OpenStreetMap contributors; Map created by @pat_macro</b>"
  ) + 
  # theme
  theme(
    axis.text.x = element_text(family = "Ubuntu", size = 140, color = "grey20", vjust = 0, hjust = 0.3), 
    axis.text.y = element_text(family = "Ubuntu", size = 140, color = "grey20", hjust = 0.6), 
    legend.background = element_blank(), 
    legend.spacing.x = unit(1, "cm"), 
    legend.key.width = unit(1.5, "cm"), 
    legend.key.height = unit(1.5, "cm"), 
    legend.title = element_blank(), 
    legend.text = element_textbox(family = "Ubuntu", size = 150, color = "black"), 
    legend.position = "bottom", 
    legend.margin = margin(5, 1.8, -5, 0.2, "cm"), 
    panel.background = element_rect(fill = "#abaeb0", color = "black"), 
    panel.grid.major = element_line(color = "black", size = 0.5), 
    plot.background = element_rect(fill = "#c0c6c9"), 
    plot.title = element_textbox(family = "Ubuntu", size = 420, color = "#2b2b2b", fill = NA, lineheight = 0.35, margin = margin(-3, 0, 3, 0, "cm"), hjust = 0.5), 
    plot.subtitle = element_textbox(family = "Ubuntu", size = 180, color = "black", fill = NA, margin = margin(-2, 0, 2, 0, "cm"), hjust = 0.5), 
    plot.caption = element_textbox(family = "Ubuntu", size = 60, color = "white", fill = NA, lineheight = 0.35, margin = margin(-8.5, 0.5, 8.5, 0, "cm")), 
    plot.margin = margin(-1, 12, 1, 4, "cm")
  )


# Saving as PNG (on Windows)
ggsave(result_plot, file = "result_plot_2022_w01.png", width = 112, height = 112, units = "cm", dpi = 200)

