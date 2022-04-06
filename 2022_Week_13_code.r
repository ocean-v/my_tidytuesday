# Loading libraries
library(tidyverse)
library(ggtext)
library(showtext)


# Loading font
font_add_google("Bebas Neue", "Bebas Neue")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports <- tuesdata$sports


# Preparing data
sports2 <- sports %>% 
  group_by(year, classification_name) %>% 
  summarise(n_sports = length(unique(sports)), total_ependiture = sum(total_exp_menwomen, na.rm = TRUE), total_revenue = sum(total_rev_menwomen, na.rm = TRUE)) %>% 
  pivot_longer(cols = contains("total")) %>% 
  mutate(classification_name = factor(classification_name, levels = c("NCAA Division I-FBS", "NCAA Division I-FCS", "NCAA Division I without football", "NCAA Division II with football", "NCAA Division II without football", "NCAA Division III with football", "NCAA Division III without football", "NAIA Division I", "NAIA Division II", "NJCAA Division I", "NJCAA Division II", "NJCAA Division III", "NCCAA Division I", "NCCAA Division II", "USCAA", "CCCAA", "NWAC", "Independent", "Other")))


# Plotting
result_plot <- ggplot(data = sports2) + 
  geom_col(aes(x = classification_name , y = value, fill = name), position = "dodge") + 
  scale_y_continuous(name = "USD") + 
  scale_fill_manual(values = c("#ee827c", "#c7dc68"), label = c("EXPENDITURES", "REVENUE")) + 
  facet_wrap(~ year, ncol = 1) + 
  labs(
    title = "Collegiate Sports' <br>Total <span style=color:'#ee827c'>Expenditures</span> & <span style=color:'#c7dc68'>Revenue</span> in USD<br>by School Categories", 
    caption = "Data comes from Equity in Athletics Data Analysis(https://ope.ed.gov/athletics/#/datafile/list); Image created by @pat_macro"
  ) + 
  theme(
    axis.title = element_blank(),  
    axis.text.x = element_text(family = "Bebas Neue", color = "white", size = 95, angle = -45, hjust = 0), 
    axis.text.y = element_text(family = "Bebas Neue", color = "white", size = 95), 
    panel.background = element_rect(fill = "#6d90b3", color = "#6d90b3"), 
    panel.grid.major.x = element_blank(), 
    plot.background = element_rect(fill = "#6d90b3", color = "#6d90b3"),   
    plot.title = element_textbox(family = "Bebas Neue", color = "white", size = 180, lineheight = 0.25, margin = margin(-1, 0, 1, 0, "cm")), 
    plot.caption = element_text(family = "Bebas Neue", color = "white", size = 40, vjust = -50), 
    plot.margin = margin(2, 2, 2, 2, "cm"), 
    legend.position = "none", 
    strip.background = element_rect(fill = NA, color = "white", size = 2), 
    strip.text = element_text(family = "Bebas Neue", color = "white", size = 150)
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 26, dpi = 300 )


