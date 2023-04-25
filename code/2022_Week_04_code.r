# Loading libraries
library(tidyverse)
library(patchwork)
library(showtext)
library(ggtext)
library(ragg)


# Loading fonts
font_add_google("Lato", "Lato")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-01-25')

ratings <- tuesdata$ratings

details <- tuesdata$details


# Preparing data
full_join(ratings, details, by = "id") %>% 
  mutate(category = {str_replace_all(boardgamecategory, pattern = "\\[|\\]", replacement = "") %>% str_split(pattern = ", ")}) %>% 
  mutate(mechanic = {str_replace_all(boardgamemechanic, pattern = "\\[|\\]", replacement = "") %>% str_split(pattern = ", ")}) %>% 
  mutate(designer = {str_replace_all(boardgamedesigner, pattern = "\\[|\\]", replacement = "") %>% str_split(pattern = ", ")}) %>% 
  mutate(artist = {str_replace_all(boardgameartist, pattern = "\\[|\\]", replacement = "") %>% str_split(pattern = ", ")}) %>% 
  mutate(minage = paste0(minage, " y/o")) %>% 
  mutate(playingtime_cat = case_when(
    (playingtime <= 15) ~ "< 15 min", 
    (15 < playingtime)&(playingtime <= 30) ~ "15-30 min", 
    (30 < playingtime)&(playingtime <= 45) ~ "30-45 min", 
    (45 < playingtime)&(playingtime <= 60) ~ "45-60 min", 
    (60 < playingtime)&(playingtime <= 90) ~ "1-1.5 hours", 
    (90 < playingtime)&(playingtime <= 120) ~ "1.5-2 hours", 
    (120 < playingtime)&(playingtime <= 150) ~ "2-2.5 hours", 
    (150 < playingtime)&(playingtime <= 180) ~ "2.5-3 hours", 
    (180 < playingtime)&(playingtime <= 240) ~ "3-4 hours", 
    (240 < playingtime) ~ "over 4 hours", 
    is.na(playingtime) ~ "No data"
  )) -> boardgame1


process_data_for_plot1 <- function(data, param) {
  param_sym <- rlang::sym(param)
  
  data %>% 
    unnest(!!param_sym) %>% 
    mutate(!!param := str_replace_all(!!param_sym, pattern = "'", replacement = "")) %>% 
    group_by(!!param_sym) %>% 
    nest() %>% 
    mutate(
      mean_average = map_dbl(data, function(data) {mean(data$average)}), 
      n_average = map_dbl(data, function(data) {length(data$average)})
    ) %>% arrange(desc(mean_average)) %>% 
    ungroup() %>% 
    mutate(!!param := factor(!!param_sym, levels = rev(!!param_sym))) %>% 
    slice(1:10) -> tmp
  
  return(list(tmp, param))
}

process_data_for_plot2 <- function(data, param) {
  param_sym <- rlang::sym(param)
  
  data %>% 
    unnest(!!param_sym) %>% 
    group_by(!!param_sym) %>% 
    nest() %>% 
    mutate(
      mean_average = map_dbl(data, function(data) {mean(data$average)}), 
      n_average = map_dbl(data, function(data) {length(data$average)})
    ) %>% arrange(desc(mean_average)) %>% 
    ungroup() %>% 
    mutate(!!param := factor(!!param_sym, levels = rev(!!param_sym))) %>% 
    slice(1:10) -> tmp
  
  return(list(tmp, param))
}

plot_tmp <- function(ttt, color, title){
  data <- ttt[[1]]
  param_sym <- rlang::sym(ttt[[2]])
  
  g <- ggplot(data = data) + 
    geom_point(aes(x = !!param_sym, y = mean_average), size = 5.1, color = color) + 
    geom_text(aes(x = !!param_sym, y = mean_average, label = n_average), size = 5, color = "white", fontface = "bold") + 
    scale_x_discrete(position = "top") + 
    scale_y_continuous(name = "Mean average rating", limits = c(5.5, 10.5)) + 
    labs(title = title) + 
    coord_flip() + 
    theme(
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      axis.text.x = element_text(family = "Lato", face = "bold", size = 28, color = "black"), 
      axis.text.y = element_text(family = "Lato", face = "bold", size = 27, color = "black", hjust = 0), 
      axis.ticks = element_blank(),
      panel.background = element_blank(), 
      panel.grid.major.x = element_line(color = "black", size = 0.1),
      panel.grid.major.y = element_blank(), 
      plot.title = element_text(face = "bold", size = 38, color = color, hjust = 0.1)
    )
  
  return(g)
}


# Plotting
g_cat <- plot_tmp(process_data_for_plot1(boardgame1, "category"), "#028760", "Category") + theme(axis.text.x = element_blank())

g_mech <- plot_tmp(process_data_for_plot1(boardgame1, "mechanic"), "#007bbb", "Mechanic")

g_age <- plot_tmp(process_data_for_plot2(boardgame1, "minage"), "#f08300", "Minimum age") + theme(axis.text.x = element_blank())

g_play <- plot_tmp(process_data_for_plot2(boardgame1, "playingtime_cat"), "#d9333f", "Playing time")


design <- c("
13
24
")

result_plot <- g_cat + g_mech + g_age + g_play + 
  plot_layout(design = design) + 
  plot_annotation(
    title = "<b>Top 10 board game attributes</b> <br><span style = 'font-size:40pt'>which have high averaged average rating values</span>", 
    caption = "\\* The number on the point indicates the number of board games which are assigned for each attribute<br>
    Data from Kaggle by way of Board Games Geek; Image created by @pat_macro", 
    theme = theme(
      plot.title = element_textbox(family = "Lato", size = 65, lineheight = 0.2), 
      plot.subtitle = element_textbox(family = "Lato", size = 45, hjust = 0.1), 
      plot.caption = element_textbox(family = "Lato", size = 20, lineheight = 0.5, hjust = 0.1)
      )
    )


# Saving iamge
ggsave(result_plot, file = "result_plot.png", width = 16, height = 14, unit = "cm", dpi = 300)

