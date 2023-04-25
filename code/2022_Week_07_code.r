# Loading libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(geomtextpath)
library(ragg)
library(magick)


# Loading fonts
font_add_google("Teko", "Teko")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-02-08')

airmen <- tuesdata$airmen
airmen2 <- airmen %>% 
  mutate(pilot_type = case_when(
    pilot_type == "Liason pilot" ~ "Liaison pilot", 
    TRUE ~ pilot_type
  )
  ) %>% 
  mutate(pilot_type = factor(pilot_type, levels = c("Service pilot", "Liaison pilot", "Single engine", "Twin engine"))) %>% 
  mutate(state = case_when(
    (state == "Unk")|is.na(state) ~ "Unk.", 
    state == "Haiti" ~ "HTI", 
    TRUE ~ state
  )
  ) %>% 
  mutate(state = factor(state, levels = sort(unique(.$state))[c(1:12, 14:40, 42:48, 13, 41)]))


# Setting colors
Dubois_black <- "#000000"
Dubois_brown <- "#654321"
Dubois_tan <- "#d2b48c"
Dubois_gold <- "#ffd700"
Dubois_pink <- "#ffc0cb"
Dubois_red <- "#dc143c"
Dubois_green <- "#00aa00"
Dubois_blue <- "#4682b4"


# Plotting
result_plot <- ggplot() + 
  geom_count(data = airmen2, aes(x = state, y = pilot_type, color = pilot_type)) + 
  scale_y_discrete(expand = c(0.05, 0.35)) + 
  scale_size_continuous(name = "Number of Airmen", range = c(6, 24)) + 
  scale_color_manual(values = c(Dubois_blue, Dubois_gold, Dubois_red, Dubois_green), guide = "none") + 
  annotate("segment", x = 1:48, xend = 1:48, y = rep(0.9, 48), yend = rep(4.7, 48), color = Dubois_black, size = 0.2) + 
  annotate("segment", x = rep(0.5, 4), xend = rep(48.5, 4), y = 1:4, yend = 1:4, color = c(Dubois_blue, Dubois_gold, Dubois_red, Dubois_green), size = 0.8) + 
  geom_count(data = airmen2, aes(x = state, y = pilot_type, color = pilot_type)) + 
  geom_textpath(aes(x = rep(0.5, 4), y = (1:4) + 0.35, label = c("Service Pilot", "Liaison Pilot", "Single Engine", "Twin Engine")), family = "Teko", color = c(Dubois_blue, Dubois_gold, Dubois_red, Dubois_green), size = 34) + 
  coord_polar(theta = "x") + 
  labs(
    title = "Tuskegee Airmen's <br>Home States & Pilot Types.", 
    caption = "Data come from GitHub (lang1023/Tuskegee-Airman-Challenge) & sourced from the CAF (Commemorative Air Force); Image created by @pat_macro"
  ) + 
  theme(
    text = element_text(family = "Teko", color = Dubois_black), 
    axis.title = element_blank(), 
    axis.text.x = element_text(color = Dubois_black, size = 100), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#d8c7b1", color = "#d8c7b1"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    plot.background = element_rect(fill = "#d8c7b1"), 
    plot.title = element_textbox(size = 200, hjust = 0.5, halign = 0.5, margin = margin(-0.5, 0.1, 0.5, 0.1, "in"), lineheight = 0.4), 
    plot.title.position = "plot", 
    plot.caption = element_textbox(size = 28, margin = margin(0.5, 0.1, -0.5, 0.1, "in")), 
    plot.caption.position = "plot", 
    plot.margin = margin(-2, 0, -2, 0, "in"), 
    legend.position = "bottom", 
    legend.background = element_blank(), 
    legend.title = element_text(size = 100), 
    legend.text = element_text(size = 100), 
    legend.key = element_rect(fill = NA)
  )


# Saving the image
ggsave(result_plot, file = "result_plot.png", width = 22, height = 28, dpi = 150, units = "in")
