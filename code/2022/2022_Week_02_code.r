# Loading libraries
library(tidyverse)
library(scatterpie)
library(patchwork)
library(showtext)
library(sf)
library(magick)

# Loading fonts
font_add_google("Acme", "Acme")
showtext_auto()

# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')

# Extracting each table
colony <- tuesdata$colony

# Preparing data
colony2 <- colony %>% 
  filter(!(state %in% c("United States", "Other States"))) %>% 
  mutate(months2 = case_when(
    months == "January-March" ~ 1, 
    months == "April-June" ~ 2, 
    months == "July-September" ~ 3, 
    months == "October-December" ~ 4
  )) %>% 
  mutate(ym = map2_chr(year, months2, paste, sep = "_"), colony_survived = colony_max - colony_lost) %>% 
  select(year, months, ym, state, colony_survived, colony_lost)


# Preparing map data
# data from https://public.opendatasoft.com/explore/dataset/us-state-boundaries/export/
mapdata <- read_sf("us-state-boundaries.geojson")

# Changing crs
mapmap <- mapdata %>% 
  st_transform(crs = "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Extractign center points of states
state.point1 <- mapdata %>% 
  select(centlon, centlat, name) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("centlon", "centlat"), crs = "wgs84") %>% 
  st_transform(crs = "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

state.point2 <- cbind(as.data.frame(state.point1), st_coordinates(state.point1))

state.point3 <- state.point2 %>% right_join(colony2, by = c("name" = "state"))

mapmap_main <- mapmap %>% 
  filter(name %in% c(unique(state.point3$name), "Nevada")&name != "Hawaii")
state.point_main <- state.point3 %>% filter(!name == "Hawaii")

mapmap_hawaii <- mapmap %>% filter(name == "Hawaii")
state.point_hawaii <- state.point3


# Plotting
# Defining theme
theme_tmp <- theme(
  axis.title = element_blank(), 
  axis.text = element_text(family = "Acme", size = 30), 
  axis.ticks = element_blank(), 
  panel.background = element_rect(fill = "#fddea5", color = NA),  
  plot.background = element_rect(fill = "#fddea5", color = NA), 
  plot.title = element_text(family = "Acme", size = 32, hjust = 0.8), 
  legend.position = c(0.9, 0.2), 
  legend.background = element_blank(), 
  legend.title = element_blank(), 
  legend.text = element_text(family = "Acme", size = 20), 
  legend.key.width = unit(0.3, "cm"), 
  legend.key.height = unit(0.3, "cm")
)

# Defining loop counter
# Caution: 2019_2 is missing
counter <- state.point_main %>% drop_na() %>% pull(ym) %>% unique()

# Plotting images
for (ii in counter) {
  state.point_main_tmp <- filter(state.point_main, ym == ii)
  state.point_hawaii_tmp <- filter(state.point_hawaii, ym == ii)
  
  # Plotting U.S. Mainland
  g1 <- ggplot() + 
    geom_sf(data = mapmap_main, fill = "#e5e4e6", color = "black", size = 0.1) + 
    geom_scatterpie(data = state.point_main_tmp, aes(x = X, y = Y, group = name), color = NA, cols = c("colony_survived", "colony_lost")) + 
    scale_fill_manual(values = c("#f8b500", "#595857")) + 
    labs(title = "Percentage of honey bee colony lost in three months in the U. S.") + 
    annotate("text", x = st_bbox(mapmap_main)[3] - 1800000, y = st_bbox(mapmap_main)[4], 
             label = paste(state.point_main_tmp$year, state.point_main_tmp$months, sep = " "), 
             family = "Acme", color = "black", size = 8, hjust = 0)  + 
    theme_tmp
  
  # Plotting Hawaii islands
  g2 <- ggplot() + 
    geom_sf(data = mapmap_hawaii, fill = "#e5e4e6", color = "black", size = 0.1) + 
    geom_scatterpie(data = state.point_hawaii_tmp, aes(x = X, y = Y, group = name), color = NA, cols = c("colony_survived", "colony_lost")) + 
    scale_fill_manual(values = c("#f8b500", "#595857")) +  
    coord_sf(xlim = st_bbox(mapmap_hawaii)[c(1, 3)], ylim = st_bbox(mapmap_hawaii)[c(2, 4)]) + 
    theme_tmp + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      panel.border = element_rect(fill = NA, color = "black", size = 0.1), 
      legend.position = "none"
    )
  
  # Patchworking two plots
  g <- g1 + 
    inset_element(g2, left = 0, bottom = 0, right = 0.35, top = 0.35, align_to = 'full') + 
    plot_annotation(
      caption = "Data derived from USDA(https://usda.library.cornell.edu/concern/publications/rn301137d?locale=en)\nImage created by @pat_macro", 
      theme = theme(
        panel.background = element_rect(fill = "#fddea5", color = NA), 
        plot.background = element_rect(fill = "#fddea5", color = NA))
      )
  
  # Saving image
  ggsave(g, file = paste0("g_", ii, ".png"), width = 10.5, height = 7, unit = "cm", dpi = 300)
}


# Reading images & saving as GIF animation
# reference: https://stackoverflow.com/questions/49612276/how-can-i-image-read-multiple-images-at-once
image_names <- list.files(".", pattern = "g_[0-9_]+.png")
images <- map(image_names, image_read) %>% image_join()
image_animate(images, fps = 1, optimize = TRUE) %>% image_write("anime.gif")

