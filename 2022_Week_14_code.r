# Loading libraries
library(tidyverse)
library(ggtext)
library(showtext)


# Loading font
font_add_google("Oswald", "Oswald")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-04-05')

news_orgs <- tuesdata$news_orgs


# Preparing data
news_orgs2 <- news_orgs %>% 
  rowwise() %>% 
  mutate(media_list = str_split(distribution, pattern = ", ")) %>% 
  ungroup() %>% 
  unnest(media_list)

media_order <- news_orgs2 %>% 
  count(media_list) %>% 
  arrange(desc(n)) %>% 
  pull(media_list) %>% 
  na.omit()

media_order2 <- c(media_order, "No data")

news_orgs3 <- news_orgs2 %>% 
  mutate(
    media_name = {
      media_list %>% 
        replace_na("No data") %>% 
        factor(levels = media_order2)
    }
  )


# Plotting
result_plot <- ggplot(data = news_orgs3) + 
  geom_jitter(
    aes(x = year_founded, y = media_name, color = media_name), 
    position = position_jitter(height = 0.3, width = 0, seed = 123), size = 1, alpha = 0.55
  ) + 
  annotate("text", x = 1945, y = 1:14 + 0.4, label = rev(media_order2), family = "Oswald", fontface = "bold", size = 14, hjust = 0, color = rev(MetBrewer::met.brewer(name = "Juarez", n = 14))) + 
  scale_x_continuous(name = "Year organisations founded", breaks = seq(1960, 2020, 20)) +  
  scale_y_discrete(limits = rev, position = "right") + 
  scale_color_manual(values = MetBrewer::met.brewer(name = "Juarez", n = 14)) + 
  coord_cartesian(xlim = c(1955, 2022), clip = "off") + 
  labs(
    title = toupper("Distribution Way of <br>Digitally Focused, Local News"), 
    subtitle = "Points in each group show the news organizations that use that way.<br> The x-axis shows the year when organizations were founded. *One organization sometimes uses several ways.", 
    caption = "Data comes from Project Oasis (https://www.projectnewsoasis.com/publications); Image created by @pat_macro"
  ) + 
  theme(
    axis.title = element_blank(), 
    axis.text.x = element_text(family = "Oswald", face = "bold", color = "#2b2b2b", size = 40), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#e8d3c7", color = "#e8d3c7"), 
    panel.grid.major.x = element_line(color = "white", linetype = 2, size = 0.5), 
    panel.grid.minor.x = element_line(color = "white", linetype = 3, size = 0.5), 
    panel.grid.major.y = element_blank(), 
    plot.background = element_rect(fill = "#e8d3c7", color = "#e8d3c7"), 
    plot.margin = margin(0.5, 0.2, 1, 2.2, "cm"), 
    plot.title = element_textbox(family = "Oswald", face = "bold", color = "#2b2b2b", size = 85, hjust = 1, vjust = 1.4, lineheight = 0.3, halign = 0), 
    plot.subtitle = element_textbox(family = "Oswald", color = "#2b2b2b", size = 28, hjust = 1.7, lineheight = 0.4, halign = 0), 
    plot.caption = element_text(family = "Oswald", color = "#2b2b2b", size = 15, lineheight = 0.4, vjust = -28), 
    legend.position = "none"
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 18, dpi = 300, unit = "cm")

