# Loading libraries
library(tidyverse)
library(patchwork)


# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')

spiders <- tuesdata$spiders


# Extracting initial letters
spiders2 <- spiders %>% 
  mutate(genus_initial = str_extract(genus, "^[A-Z]"), 
         specific_initial = str_extract(species, "^[a-z]{1}")
  )


# Counting number of species
spiders2 %>% group_by(genus_initial, specific_initial) %>% tally() -> spiders_initial


# Plotting
# Main heat map
main_heat <- ggplot() + 
  geom_point(data = spiders_initial, aes(x = specific_initial, y = genus_initial, size = n, color = n)) + 
  xlab("a ↔ z") + ylab("Z ↔ A") + 
  scale_x_discrete(position = "bottom") + 
  scale_y_discrete(limits = rev) + 
  scale_size_continuous(name = "Number of Species", trans = "log10") + 
  scale_color_viridis_c(name = "Number of Species", option = "magma", trans = "log10", direction = -1) +
  guides(size = guide_legend(), color = guide_legend()) + 
  theme(
    axis.title.x = element_text(family = "serif", face = "bold", color = "white", size = 24), 
    axis.title.y = element_text(family = "serif", face = "bold", color = "white", size = 20), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.background = element_rect(fill = "#474a4d"), 
    legend.title = element_text(family = "serif", face = "bold", color = "white", size = 12), 
    legend.text = element_text(family = "serif", color = "white", size = 15), 
    legend.key = element_rect(fill = "#474a4d"), 
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    panel.background = element_rect(fill = "#474a4d"), 
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = "#474a4d", color = "#474a4d"), 
    plot.title = element_blank(), 
    plot.margin = margin(-2, -0.5, -4, 1.2, "cm")
  )
plot(main_heat)


# Upper side barplot
spiders2 %>% group_by(specific_initial) %>% tally() -> spiders_specific_initial

upperside_bar <- ggplot() + 
  geom_bar(data = spiders_specific_initial, aes(x = specific_initial, y = n, fill = n), stat = "identity") + 
  scale_fill_viridis_c(option = "viridis", direction = -1) + 
  scale_y_continuous(breaks = c(0, 2000, 4000)) + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_text(color = "white"), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_line(color = "white"), 
        panel.background = element_rect(fill = "#474a4d"), 
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "#474a4d", color = "#474a4d"), 
        plot.margin = margin(-2, 0, -4, 0, "cm"), 
        legend.position = "none"
  )


# Upper side blank plot
upperside_blank <- ggplot() + 
  theme(
    panel.background = element_rect(fill = "#474a4d"), 
    plot.background = element_rect(fill = "#474a4d", color = "#474a4d")
  )


# Leftside barplot
spiders2 %>% group_by(genus_initial) %>% tally() -> spiders_genus_initial

leftside_hist <- ggplot() + 
  geom_bar(data = spiders_genus_initial, aes(x = genus_initial, y =n, fill = n), stat = "identity") + 
  scale_x_discrete(limits = rev) + 
  scale_fill_viridis_c(option = "viridis", direction = -1) + 
  scale_y_continuous(breaks = c(0, 2000, 4000), position = "left") + 
  coord_flip() + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(color = "white", hjust = 0, vjust = 0.5, angle = -90), 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_line(color = "white"), 
        axis.ticks.y = element_blank(), 
        panel.background = element_rect(fill = "#474a4d"), 
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "#474a4d", color = "#474a4d"), 
        plot.margin = margin(-3, 1, -3,0, "cm"), 
        legend.position = "none"
  )




# patchworking
patch_design <- "
  34
  12
"

width_ratio <- 7
height_ratio <- 7.5

result_plot <- main_heat + leftside_hist + upperside_bar + upperside_blank + 
  plot_layout(
    widths = c(width_ratio, 1), 
    height = c(1, height_ratio), 
    design = patch_design
  ) & 
  plot_annotation(
    title = "Spider genus / specific names start with...\n", 
    caption = "Data comes from World Spider Database(https://wsc.nmbe.ch/dataresources)/Image created by @pat_macro", 
    theme = theme(
      plot.background = element_rect(fill = "#474a4d", color = "#474a4d"), 
      plot.title = element_text(family = "serif", face = "bold.italic", color = "white", size = 25, hjust = 0.5, vjust = 0), 
      plot.caption = element_text(color = "white", size = 5, hjust = 0.5, vjust = 0), 
    )
  )


# Previewing plot
plot(result_plot)


# Saving plot
ggsave(result_plot, file = "result_plot.png", width = 7, height = 7*(height_ratio/width_ratio))
