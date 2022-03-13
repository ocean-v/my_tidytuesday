# Loading libraries
library(tidyverse)
library(showtext)
library(sf)
library(magick)


# Loading a font
font_add_google("PT Sans", "PT Sans")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations


# Preparing data
stations2 <- stations %>% 
  mutate(
    X = na_if(X, 0), 
    Y = na_if(Y, 0)
  ) %>% 
  mutate(open_year = str_extract(OPEN_DATE, pattern = "^[0-9]{4}")) %>% 
  mutate(
    open_year2 = case_when(
      is.na(open_year) ~ "NA", 
      (1970 <= open_year)&(open_year < 1980) ~ "1970s", 
      (1980 <= open_year)&(open_year < 1990) ~ "1980s", 
      (1990 <= open_year)&(open_year < 2000) ~ "1990s", 
      (2000 <= open_year)&(open_year < 2010) ~ "2000s", 
      (2010 <= open_year)&(open_year < 2020) ~ "2010s", 
      (2020 <= open_year)&(open_year < 2030) ~ "2020s", 
      TRUE ~ "NA"
    )
  ) %>% 
  mutate(
    fuel_type = as.factor(
      case_when(
        FUEL_TYPE_CODE == "BD" ~ "Biodiesel (B20 and above)", 
        FUEL_TYPE_CODE == "CNG" ~ "Compressed Natural Gas", 
        FUEL_TYPE_CODE == "ELEC" ~ "Electric", 
        FUEL_TYPE_CODE == "E85" ~ "Ethanol (E85)", 
        FUEL_TYPE_CODE == "HY" ~ "Hydrogen", 
        FUEL_TYPE_CODE == "LNG" ~ "Liquefied Natural Gas", 
        FUEL_TYPE_CODE == "LPG" ~ "Propane"
      )
    )
  )

stations2_sf <- stations2 %>% 
  filter(!(is.na(X) | is.na(Y))) %>% 
  st_as_sf(coords = c("X", "Y"), crs = "wgs84") %>% 
  st_transform(crs = "+proj=robin +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")


# Saving image
counter <- c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")

stations2_sf_ref <- bind_rows(
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "BD"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "CNG"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "ELEC"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "E85"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "HY"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "LNG"), 1), 
  slice(filter(stations2_sf, FUEL_TYPE_CODE == "LPG"), 1)
)


for (ii in counter) {
  
  stations2_sf_tmp <- stations2_sf %>% filter(open_year2 == ii)
  
  g <- ggplot() + 
    geom_sf(data = stations2_sf_ref, aes(color = fuel_type), alpha = 0) + 
    geom_sf(data = stations2_sf_tmp, aes(color = fuel_type), size = 0.1, alpha = 0.5) + 
    scale_color_manual(values = RColorBrewer::brewer.pal(name = "Set1", n = 7)) + 
    annotate("text", x = mean(st_bbox(stations2_sf)[1] + 1400000), y = mean(st_bbox(stations2_sf)[c(2, 4)]) + 400000, label = "Alternative Fuel Station Trends", family = "PT Sans", color = "white", size = 12) + 
    annotate("text", x = mean(st_bbox(stations2_sf)[1] + 1400000), y = mean(st_bbox(stations2_sf)[c(2, 4)]), label = paste0("Open year: ", ii), family = "PT Sans", color = "white", size = 30) + 
    labs(caption = "Data comes from USDOT BTS(https://data-usdot.opendata.arcgis.com/datasets/usdot::alternative-fueling-stations/about); Image created by @pat_macro") + 
    coord_sf(xlim = st_bbox(stations2_sf)[c(1, 3)], ylim = st_bbox(stations2_sf)[c(2, 4)]) + 
    guides(color = guide_legend(title.position = "top", direction = "horizontal", ncol = 2, override.aes = list(size = 1.8))) + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_text(family = "PT Sans", color = "white", size = 45), 
      panel.background = element_rect(fill = "#393e4f"), 
      panel.grid.major = element_line(size = 0.1), 
      plot.background = element_rect(fill = "#393e4f"), 
      plot.caption = element_text(family = "PT Sans", color = "white", size = 15), 
      legend.position = c(0.72, 1.06), 
      legend.background = element_blank(), 
      legend.title = element_blank(), 
      legend.text = element_text(family = "PT Sans", color = "white", size = 38), 
      legend.key = element_blank(), 
      legend.key.size = unit(0.3, "cm"), 
      legend.spacing.x = unit(0.1, "cm"), 
      legend.spacing.y = unit(2.5, "cm")
    )
  
  ggsave(g, file = paste0("g_", ii, ".png"), width = 15, height = 8, dpi = 500, units = "cm")
}


# Reading images & saving as GIF animation
# reference: https://stackoverflow.com/questions/49612276/how-can-i-image-read-multiple-images-at-once
image_names <- list.files(".", pattern = "g_[0-9]{4}s.png")
images <- map(image_names, image_read) %>% image_join() %>% image_resize("1800x960")
anime <- image_animate(images, fps = 1, optimize = TRUE)
image_write(anime, "anime.gif")
