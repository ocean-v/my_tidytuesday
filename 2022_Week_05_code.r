# Loading libraries
library(tidyverse)
library(ggtext)
library(showtext)


# Loading fonts
font_add_google("Alfa Slab One", "Alfa Slab One")

showtext_auto()


# Loading data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')


# Preparing data
breed_traits2 <- breed_traits %>% 
  mutate(Breed = str_replace_all(Breed, pattern = "\\s|’", replacement = "_")) %>% 
  select(-`Coat Type`, -`Coat Length`) %>% 
  pivot_longer(cols = !contains("Breed"))

breed_rank_all2 <- breed_rank_all %>% 
  mutate(
    Breed = str_replace_all(Breed, pattern = "\\s|’", replacement = "_"), 
   `2020_alpha_reference` = case_when(20 < `2020 Rank`&`2020 Rank` < 176 ~ 0.6, TRUE ~ 0.75), 
   `2020_size_reference` = case_when(20 < `2020 Rank`&`2020 Rank` < 176 ~ "E", TRUE ~ "I")
  )

full_join(breed_traits2, breed_rank_all2, by = "Breed") -> breed_data


result_plot <- ggplot(breed_data) + 
  geom_point(aes(x = name, y = value, color = `2020 Rank`, alpha = `2020_alpha_reference`, size = `2020_size_reference`), pch = 16, position = position_jitter(width = 0.35, height = 0.3, seed = 1)) + 
  scale_x_discrete(position = "top", limits = rev) + 
  scale_y_reverse(breaks = c(0, 1, 2, 3, 4, 5), labels = c(NA, 1, 2, 3, 4, 5)) + 
  scale_color_gradientn(colors = c("#d9333f", "#d9333f", "#f8fbf8", "#f8fbf8", "#007bbb", "#007bbb"), values = scales::rescale(rev(c(1, 20, 21, 175, 176, 195))), breaks = rev(c(1, 20, 176, 195)), trans = "reverse") + 
  scale_alpha_identity() + 
  scale_size_manual(values = c(0.3, 1.3), guide = "none") + 
  coord_flip() + 
  labs(
    title = "<span style = 'color:#007b43'>Traits</span> and <span style = 'color:#895b8a'>Popularity</span> of <span style = 'color:#946243'>Dog Breeds</span>", 
    caption = "Data comes from https://github.com/kkakey/dog_traits_AKC (originally from the American Kennel Club); Image created by @pat_macro"
  ) + 
  theme(
    text = element_text(family = "Alfa Slab One"), 
    axis.title = element_blank(), 
    axis.text.x = element_text(family = "Alfa Slab One", color = "#2b2b2b", size = 35), 
    axis.text.y = element_text(size = 28, color = "#2b2b2b"), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#c8c2c6"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_line(color = "black", size = 0.1), 
    plot.background = element_rect(fill = "#c8c2c6"), 
    plot.title = element_textbox(face = "bold", size = 70, margin = margin(-0.3, 0.6, 0.3, -0.6, "cm")), 
    plot.caption = element_text(color = "white", size = 15, hjust = -0.7, vjust = -8), 
    plot.margin = margin(0.95, 1.1, 0.4, 1.1, "cm"), 
    legend.background = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_text(size = 30), 
    legend.text = element_text(size = 30), 
    legend.key.height = unit(0.1, "cm"), 
    legend.key.width = unit(1.7, "cm"), 
    legend.box.margin = margin(-0.1, 0.5, -0.3, 4.5, "cm")
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 16, height = 14, unit = "cm", dpi = 300)

