# Loading libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(magick)


# Loading font
font_add_google("Noto Sans", "Noto Sans")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-04-12')
fuel_access <- tuesdata$fuel_access


# Saving images
for (year in unique(fuel_access$Year)) {
  result_plot <- ggplot(data = filter(fuel_access, Year == year)) + 
    geom_point(aes(x = 1, y = 1, color = `Access to clean fuels and technologies for cooking  (% of population)`), size = 10) + 
    scale_color_distiller(palette = "Spectral", direction = 1, breaks = c(0, 100), limits = c(0, 100)) + 
    facet_wrap(~ Entity) + 
    labs(
      title = paste0("Yearly Changes in Each Entities' Access to <br>Clean Fuels and Technologies for Cooking <br><b>", year, "</b> <span style=color:'#D53E4F'>0%</span>←→<span style=color:'#3288BD'>100%</span> of population"), 
      caption = "Data from Our World in Data(https://ourworldindata.org/indoor-air-pollution); Image created by @pat_macro"
    ) + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      plot.title = element_textbox(family = "Noto Sans", size = 20, lineheight = 0.6), 
      plot.caption = element_text(family = "Noto Sans", size = 6), 
      legend.position = "none", 
      strip.background = element_blank(), 
      strip.text = element_blank()
    )
  
  ggsave(result_plot, file = paste0("result_plot_", year, ".png"), width = 7, height = 8, dpi = 200, unit = "cm")
}


# Reading images & saving as GIF animation
# reference: https://stackoverflow.com/questions/49612276/how-can-i-image-read-multiple-images-at-once
image_names <- list.files(".", pattern = "result_plot_[0-9]{4}.png")
images <- map(image_names, image_read) %>% image_join()
anime <- image_animate(images, fps = 1, optimize = TRUE)
image_write(anime, "anime.gif")

