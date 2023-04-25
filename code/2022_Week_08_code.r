# Loading libraries
library(tidyverse)
library(showtext)


# Loading a font
font_add_google("Abril Fatface", "Abril Fatface")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom


# Preparing data
freedom2 <- freedom %>% 
  pivot_longer(cols = c(CL, PR)) %>% 
  mutate(name = case_when(
      name == "CL" ~ "Civil Liberties", 
      name == "PR" ~ "Political Rights"
    )
  )


# Plotting
result_plot <- ggplot(data = freedom2) + 
  geom_bar(aes(x = year, fill = as.character(value)), stat = "count", position = "fill") + 
  scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0), expand = expansion(mult = c(0, 0.01))) + 
  scale_fill_manual(values = rev(MetBrewer::met.brewer("Gauguin", 7))) + 
  facet_wrap(~ name, ncol = 1) + 
  labs(
    title = "Yearly Changes in Freedom in the World", 
    subtitle = "The lower the number is, the freer the country is.", 
    caption = "Data comes from GitHub(ArthurCheib/analytical-politics-project) and are originally derived from Freedom House(https://freedomhouse.org/reports/publication-archives) \nand the United Nations(https://unstats.un.org/unsd/methodology/m49/overview/). Image is created by @pat_macro."
  ) + 
  guides(fill = guide_legend(direction = "horizontal", nrow = 1)) + 
  theme(
    axis.title = element_blank(), 
    axis.text = element_text(family = "Abril Fatface", color = "black", size = 20), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#e6eae6"), 
    plot.background = element_rect(fill = "#e6eae6"), 
    plot.title = element_text(family = "Abril Fatface", color = "black", size = 40, hjust = 1, vjust = 1.6), 
    plot.subtitle = element_text(family = "Abril Fatface", color = "black", size = 18, hjust = 0.45, vjust = 2), 
    plot.caption = element_text(family = "Abril Fatface", color = "black", size = 7.5, hjust = 0.5, vjust = -9), 
    plot.margin =  margin(0.35, 0.75, 0.50, 0.75, "cm"), 
    legend.position = "bottom", 
    legend.direction = "horizontal", 
    legend.background = element_blank(), 
    legend.title = element_blank(), 
    legend.text = element_text(family = "Abril Fatface", color = "black", size = 20), 
    strip.background = element_blank(), 
    strip.text = element_text(family = "Abril Fatface", color = "black", size = 28)
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 16, dpi = 200, units = "cm")

