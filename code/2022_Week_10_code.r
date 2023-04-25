# Laoding libraries
library(tidyverse)
library(showtext)


# Loading a font
font_add_google("Nunito", "Nunito")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

# Preparing data
erasmus_tmp <- tibble(sending_country_code = erasmus$sending_country_code, receiving_country_code = erasmus$sending_country_code) %>% 
  expand(sending_country_code, receiving_country_code)

erasmus2 <- erasmus %>% 
  group_by(sending_country_code, receiving_country_code) %>% 
  summarise(n = n(), mean_mobility_duration = mean(mobility_duration)) %>% 
  full_join(erasmus_tmp)


# Plotting
result_plot <- ggplot(data = erasmus2) + 
  geom_point(aes(x = sending_country_code, y = receiving_country_code, color = mean_mobility_duration, size = n)) + 
  scale_color_viridis_c(name = "Mobility duration", option = "mako", breaks = c(1, 10, 30, 100, 300), trans = "log") + 
  scale_size_continuous(name = "Number of students", breaks = c(1, 20, 2000, 20000), range = c(0.5, 2.8), trans = "log") + 
  annotate("segment", x = 1, xend = 54, y = 1, yend = 54, size = 0.1, linetype = 2) + 
  labs(
    title = "Student Mobility via the ERASMUS Program", 
    subtitle = "from which country(X-axis) & to which country(Y-axis)", 
    caption = "Data comes from Data.Europa(https://data.europa.eu/data/datasets?locale=en&catalog=eac&query=erasmus&page=1&sort=issued%2Bdesc,%20relevance%2Bdesc,%20title.en%2Basc); \nImage created by @pat_macro"
  ) + 
  guides(
    color = guide_colorbar(barwidth = unit(5, "cm"), barheight = unit(0.65, "cm"), title.position = "top"), 
    size = guide_legend(color = "#19448e", title.position = "top", label.position = "bottom")
  ) + 
  theme(
    axis.title = element_blank(), 
    axis.text.x = element_text(family = "Nunito", face = "bold", color = "#19448e", size = 16, angle = -90, hjust = 0, vjust = 0.5), 
    axis.text.y = element_text(family = "Nunito", face = "bold", color = "#19448e", size = 16), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = NA), 
    panel.border = element_rect(fill = NA, color = "#19448e"), 
    panel.grid.major = element_blank(), 
    plot.background = element_rect(fill = "white"), 
    plot.margin = margin(0.75, 0.75, 0.75, 0.75, unit = "cm"), 
    plot.title = element_text(family = "Nunito", face = "bold", color = "#19448e", size = 32, hjust = 0.5), 
    plot.subtitle = element_text(family = "Nunito", color = "#19448e", size = 24, hjust = 0.5), 
    plot.caption = element_text(family = "Nunito", color = "#19448e", size = 8, hjust = 1), 
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.title = element_text(family = "Nunito", face = "bold", color = "#19448e", size = 24), 
    legend.title.align = 0.5, 
    legend.text = element_text(family = "Nunito", color = "#19448e", size = 24), 
    legend.key = element_blank()
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 19, unit = "cm", dpi = 200)


