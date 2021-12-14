library(tidyverse)
library(extrafont)
library(patchwork)
# and using package 'hadley/emo'


# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-12-14')


# Detecting characters & doing logical calculation
Lyrics_i_you <- tuesdata$lyrics %>% 
  mutate(song_name_abv = abbreviate(song_name), 
         existence_of_I = str_detect(line, " I | I'|I |I'| I[,|.]| [M|m]e |Me | [M|m]e[,|.]"), 
         existence_of_you = str_detect(line, " [Y|y]ou | [Y|y]ou'|You |You'| [Y|y]ou[,|.]"), 
         containing_I_and_you = (existence_of_I&existence_of_you), 
         containing_only_I = (existence_of_I&(!existence_of_you)), 
         containing_only_you = ((!existence_of_I)&existence_of_you), 
         containing_neither = ((!existence_of_I)&(!existence_of_you))
  )


# Summarising by song names & transforming the table into longer table
Lyrics_i_you2 <- Lyrics_i_you %>% 
  group_by(song_name_abv) %>% 
  summarise_at(vars(contains("containing")), sum) %>% 
  mutate(count_all = containing_I_and_you + containing_only_I + containing_only_you + containing_neither) %>% 
  pivot_longer(cols = contains("containing"), names_to = "category", values_to = "count")


# Releveling categories & rearranging
f_level <- rev(c("containing_only_I", "containing_I_and_you", "containing_only_you", "containing_neither"))

Lyrics_i_you3 <- Lyrics_i_you2 %>% 
  filter(category != "count_all") %>% 
  mutate(category = factor(category, levels = f_level)) %>% 
  arrange(desc(count_all))


# Releveling song names & calculating percentages
Lyrics_i_you3$song_name_abv %>% unique() -> name_levels

Lyrics_i_you4 <- Lyrics_i_you3 %>% 
  mutate(song_name_abv = factor(song_name_abv, levels = name_levels)) %>% 
  mutate(count_percentage = count/count_all)


# Defining a function for musical notes
rnorm_seed1 <- function(seed = seed, mean = mean, sd = sd, n = n) {
  set.seed(seed)
  v <- rnorm(mean = mean, sd = sd, n = n)
  return(v)
}


# Plotting
# Bar plot 1(for showing counts)
count_bar_plot <- ggplot(Lyrics_i_you4) + 
  geom_bar(aes(x = song_name_abv, y = count, fill = category), stat = "identity") + 
  scale_fill_manual(
    values = c("Grey50", "#2ca9e1", "#884898", "#e95295"), 
    labels = c("Containing neither", "Containing only 'you", "Containing 'I/me' and 'you'", "Containing only 'I/me'"), 
    guide = guide_legend(reverse = TRUE)) + 
  annotate("text", 
           x = seq(10, 18, by = 4), 
           y = 95 + rnorm_seed1(seed = 9, mean = 0, sd = 8, n = 3), 
           angle = 0 + rnorm_seed1(seed = 4, mean = 0, sd = 10, n = 3), 
           label = emo::ji("musical_note"), 
           size = 15 + rnorm_seed1(seed = 7, mean = 0, sd = 1, n = 3)) + 
  ylab("Number of lines") + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(family = "Segoe UI", face = "bold", size = 45), 
    axis.text.x = element_blank(), 
    axis.text.y = element_text(family = "Segoe UI", face = "bold", size = 40), 
    axis.ticks.x = element_blank(), 
    legend.background = element_blank(), 
    legend.title = element_blank(), 
    legend.text = element_text(family = "Segoe UI", size = 30), 
    legend.key = element_rect(fill = NA), 
    legend.position = c(0.85, 0.85), 
    panel.background = element_rect(fill = "white", color = "white"), 
    plot.background = element_rect(fill = "white", color = "white")
  )


#  Bar plot 2(for showing percentages)
# Finding No1 song among each categories
Containing_you_No.1 <- Lyrics_i_you4 %>% 
  filter(category %in% c("containing_only_you", "containing_I_and_you")) %>% 
  group_by(song_name_abv) %>% 
  summarize(sum_you = sum(count_percentage)) %>% 
  arrange(desc(sum_you)) %>% 
  as.data.frame() %>% 
  magrittr::extract(1, 1) %>% 
  as.character()

Containing_I_No.1 <- Lyrics_i_you4 %>% 
  filter(category %in% c("containing_only_I", "containing_I_and_you")) %>% 
  group_by(song_name_abv) %>% 
  summarize(sum_I = sum(count_percentage)) %>% 
  arrange(desc(sum_I)) %>% 
  as.data.frame() %>% 
  magrittr::extract(1, 1) %>% 
  as.character()

Containing_I_or_you_No.1 <- Lyrics_i_you4 %>% 
  filter(category %in% c("containing_only_I", "containing_I_and_you", "containing_only_you")) %>% 
  group_by(song_name_abv) %>% 
  summarize(sum_I_or_you = sum(count_percentage)) %>% 
  arrange(desc(sum_I_or_you)) %>% 
  as.data.frame() %>% 
  magrittr::extract(1, 1) %>% 
  as.character()


# Plot
percentage_bar_plot <- ggplot(Lyrics_i_you4) + 
  geom_bar(aes(x = song_name_abv, y = count_percentage, fill = category), stat = "identity") + 
  scale_y_continuous(breaks = c(0.0, 0.5, 1.0)) + 
  scale_fill_manual(values = c("Grey50", "#2ca9e1", "#884898", "#e95295")) + 
  annotate("text", 
           x = c(Containing_you_No.1, Containing_I_or_you_No.1, Containing_I_No.1), 
           y = c(1.1, 1.2, 1.1), 
           label = c("○", "☆", "●"), 
           size = 15, hjust = 0.5) + 
  annotate("segment", 
           x = c(Containing_you_No.1, Containing_I_or_you_No.1, Containing_I_No.1), 
           xend = c(Containing_you_No.1, Containing_I_or_you_No.1, Containing_I_No.1), 
           y = 1.0, 
           yend = c(1.1, 1.1, 1.1) - 0.06, 
           color = "grey30") + 
  annotate("text", 
           x = 1, 
           y = c(1.13, 1.09, 1.05), 
           label = c("○: The song whose percentage of lines containing only 'you' is the highest", 
                     "☆: The song whose percentage of lines containing 'I/me' or 'you' is the highest", 
                     "●: The song whose percentage of lines containing only 'I/me' is the highest"), 
           size = 4, hjust = 0) + 
  ylab("Percentage") + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(family = "Segoe UI", face = "bold", size = 45, hjust = 0.3), 
    axis.text.x = element_text(family = "Segoe UI", face = "bold", size = 35, hjust = 1, vjust = 0.5, angle = 90), 
    axis.text.y = element_text(family = "Segoe UI", face = "bold", size = 40), 
    axis.ticks.x = element_blank(), 
    panel.background = element_rect(fill = "white", color = "white"), 
    plot.background = element_rect(fill = "white", color = "white"),
    legend.position = "none"
  )


# Patchworking
design <- "
1
2
"

result_plot <- count_bar_plot + percentage_bar_plot + 
  plot_layout(design = design) & 
  plot_annotation(
    title = "Does a song line contain 'I/me', 'You' or both?\n", 
    caption = "Data provided by Jacquie Tran on GitHub and derived from Spotify and Genius / Image created by @pat_macro", 
    theme = theme(
      plot.title = element_text(family = "Gill Sans Ultra Bold Condensed", face = "bold.italic", size = 70, vjust = 1, hjust = 0.35), 
      plot.caption = element_text(family = "Segoe UI", size = 12, vjust = -10), 
      plot.margin = margin(4, 4, 2, 2, "cm")
    )
  )

ggsave(result_plot, file = "week51_result_plot.png", width = 20, height = 20)

