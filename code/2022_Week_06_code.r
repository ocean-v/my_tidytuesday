# Loading libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(geomtextpath)
library(ragg)


# Loading fonts
font_add_google("Special Elite", "Special Elite")

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


# Plotting
result_plot <- ggplot() + 
  geom_count(data = airmen2, aes(x = state, y = pilot_type), color = "white") + 
  scale_y_discrete(expand = c(0.05, 0.35)) + 
  scale_size_continuous(name = "Number of Airmen", range = c(3, 12)) + 
  annotate("segment", x = 1:48, xend = 1:48, y = rep(0.9, 48), yend = rep(4.7, 48), color = "white", size = 0.1) + 
  annotate("segment", x = rep(0.5, 4), xend = rep(48.5, 4), y = 1:4, yend = 1:4, color = "white", size = 0.3) + 
  geom_textpath(aes(x = rep(0.5, 4), y = (1:4) + 0.35, label = c("Service Pilot", "Liaison Pilot", "Single Engine", "Twin Engine")), family = "Special Elite", color = "white", size = 27) + 
  coord_polar(theta = "x") + 
  labs(
    title = "Tuskegee Airmen's Home States & Pilot Types", 
    caption = "Data come from GitHub (lang1023/Tuskegee-Airman-Challenge) & sourced from the CAF (Commemorative Air Force); Image created by @pat_macro"
  ) + 
  theme(
    text = element_text(family = "Special Elite", color = "white"), 
    axis.title = element_blank(), 
    axis.text.x = element_text(color = "white", size = 80), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#0073a8", color = "#0073a8"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    plot.background = element_rect(fill = "#0073a8"), 
    plot.title = element_textbox(size = 100, hjust = 0.55, lineheight = 0.4), 
    plot.caption = element_text(size = 28), 
    legend.position = "bottom", 
    legend.background = element_blank(), 
    legend.title = element_text(size = 60), 
    legend.text = element_text(size = 60), 
    legend.key = element_rect(fill = NA)
  )


# Saving the image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 14, dpi = 300)

