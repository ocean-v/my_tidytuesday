# Loading libraries
library(tidyverse)
library(tidytuesdayR)


# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata <- tidytuesdayR::tt_load(2021, week = 49)
matches <- tuesdata$matches


# Function definition(to use to create combination)
names_sort <- function(A, B){
  tmp <- c(A, B) %>% str_sort() %>% paste0(collapse = "_")
  return(tmp)
}

# Reference data for team combination
teams <- c(matches$team1, matches$team2) %>% unique()
tibble(teamA = teams, teamB = rev(teams)) -> teamS
teamS %>% expand(teamA, teamB) %>% mutate(comb = map2_chr(teamA, teamB, names_sort)) -> team_combination


# Data preparation
matches %>% 
  mutate(winner2 = case_when(winner == "U.A.E." ~ "United Arab Emirates", winner == "Pakistan awarded the match (opposition conceded)" ~ "Pakistan", TRUE ~ winner)) %>% 
  mutate(comb = map2_chr(team1, team2, names_sort)) -> matches_v1


# Grouping each team combination
matches_v1 %>% 
  group_by(comb) %>% nest() %>% 
  mutate(no_win = map(data, function(data1){
    data1 %>% group_by(winner2) %>% tally() -> xx
    return(xx)
  }
  )
  ) -> matches_v2


# Calculating No. of wins minus No. of losses 
full_join(team_combination, matches_v2) %>% 
  mutate(dif_win = pmap_int(list(teamA, teamB, no_win), function(tA, tB, data2){
    if(!is.null(data2)) {
      data2 %>% filter(winner2 == tA) %>% pull(n) -> n_A
      data2 %>% filter(winner2 == tB) %>% pull(n) -> n_B
      if (!is_empty(n_A)&!is_empty(n_B)) {
        yy <- n_B - n_A
      } else if (is_empty(n_A)) {
        yy <- n_B
      } else if (is_empty(n_B)) {
        yy <- -n_A
      }  
      return(yy)
    } else {
      return(NA)
    }
  }
  )
  ) -> matches_v3


# Getting a vector for team name arrangement
matches_v3 %>% drop_na() %>% count(teamA) %>% arrange(desc(n)) %>% pull(teamA) -> facref
# Creating a vector for plot label
labref <- facref; labref[13] <- "U.A.E."; labref[20] <- "U.S.A."
# Factor relevel
matches_v3 %>% 
  mutate(teamA = factor(teamA, levels = facref), teamB = factor(teamB, levels = rev(facref))) -> matches_v4


# Texts for plot
ttl <- "   Did a team have more wins than losses?"
anot <- "The number above each cell indicates \nNo. of wins minus No. of losses \nof a team listed on the left \nat each combination (not concerning tie)."
cap <- "Data comes from ESPN Cricinfo by way of Hassanasir/Image created by @pat_macro"


# Creating ggplot
ggplot(matches_v4) + 
  geom_raster(aes(x = teamA, y = teamB, fill = dif_win)) + 
  scale_x_discrete(position = "top", labels = str_extract(facref, "[A-Z]{1}")) + 
  scale_y_discrete(labels = rev(labref)) + 
  scale_fill_gradientn(c(-25, 0, 25), colors = c("blue", "grey", "red"), na.value = rgb(0, 0, 0, alpha = 0)) + 
  geom_text(aes(x = teamA, y = teamB, label = dif_win), size = 3, color = "#595857") + 
  annotate("label", x = 4.7, y = 2.5, label = anot, family = "mono", size = 2.8, color = "#e6b422", fill = "#e6b422", hjust = 0) + 
  annotate("text", x = 4.7, y = 2.5, label = anot, family = "mono", size = 2.8, color = "white", fontface = "bold", hjust = 0) + 
  labs(title = ttl, caption = cap) + 
  theme(
    panel.background = element_rect(fill = "#e6b422"), 
    panel.grid = element_line(linetype = 3, size = 0.5), 
    plot.background = element_rect(fill = "#e6b422"), 
    plot.title.position = "plot", 
    plot.title = element_text(family = "mono", size = 18, face = "bold", color = "#2c4f54"), 
    plot.caption = element_text(color = "white"),  
    legend.position = "none", 
    axis.text.x = element_text(face = "bold", color = "white", size = 10), 
    axis.text.y = element_text(face = "bold", color = "white", size = 11), 
    axis.title = element_blank(), 
    plot.margin = margin(0.5, 1, 1, 1, "cm")) -> g_result


# Preview
plot(g_result)
# Save as PNG
ggsave(g_result, file = "g_result.png", width = 7, height = 7)
