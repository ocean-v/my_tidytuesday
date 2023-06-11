# Loading libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(magick)


# Loading font
font_add_google("Noto Sans", "Noto Sans")
showtext_auto()


# Loading data
owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')


# Shaping data
owid_energy


owid_energy %>% ggplot(data = .) + geom_point(aes(x = energy_per_capita, y = fossil_elec_per_capita, color = country)) + theme(legend.position = "none")