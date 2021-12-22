# Loading libraries
library(tidyverse)
library(ggtext)
library(showtext)


# Loading fonts
font_add_google("Work Sans", "WSans")
font_add_google("Sarina", "Sarina")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-12-21')

starbucks <- tuesdata$starbucks


# Preparing data
# Extracting highest values
starbucks %>% 
  group_by(product_name) %>% 
  summarise_at(vars(c(calories, caffeine_mg)), max) -> max_cal_caf_df

# Forming name-word matrix
starbucks %>% 
  select(product_name) %>% 
  distinct() %>% 
  mutate(name_element = str_split(tolower(product_name), " "), n = 1) %>% 
  unnest(name_element) %>% 
  filter(!(name_element %in% c("-", "&", ""))) %>% 
  pivot_wider(names_from = name_element, values_from = n, values_fn = sum, values_fill = 0) -> names_df
names_df %>% 
  column_to_rownames("product_name") -> names_mat

# Doing PCA
prcomp(names_mat) -> names_pca

# Extracting principal component values
left_join(names_df, max_cal_caf_df, by = "product_name") %>% bind_cols(as.data.frame(names_pca$x)) -> names_pca.x
names_pca$rotation %>% as.data.frame() %>% rownames_to_column("word") -> names_pca.rot

names_pca.rot %>% 
  mutate(rank_ref = sqrt(PC1^2 + PC2^2)) %>% 
  arrange(desc(rank_ref)) %>% 
  slice(1:8) -> names_pca.rot_top8


# Plotting
Title <- "The Relationship between <br>Names and Nutrition"

Caption <- c(
"<b>Beverages are projected on the first and the second principal component extracted from <br>
the principal component analysis using words contained in each beverage's name as variables(0, 1). <br>
Points represent beverages, the size of points represents calories of beverages and the color of <br>
points represents caffeine contained in beverages. The highest values of calorie and caffeine <br>
in each beverage are selected. Arrows and words show the original variables which are projected <br>
on to the principal components (the top eight words ranked by sqrt(PC1<sup>2</sup> + PC2<sup>2</sup>) are selected to show). <br></b>
<br>
Data from GitHub(PythonCoderUnicorn/starbucks), originally derived <br>
&emsp; from the pdf *Starbucks Coffee Company* Beverage Nutrition Information; Image by @pat_macro"
)


result_plot <- ggplot() + 
  geom_point(data = names_pca.x, 
             aes(x = PC1, y = PC2, size = calories, color = caffeine_mg), alpha = 0.8) + 
  geom_segment(data = names_pca.rot, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), size = 1.2, color = "#007b43", arrow = arrow(length = unit(0.3, "cm")), alpha = 0.7) + 
  geom_text(data = names_pca.rot_top8, 
            aes(x = PC1, y = PC2, label = word, size = rank_ref, alpha = rank_ref), family = "WSans", fontface = "bold", size = 45, color = "#007b43") + 
  annotate("text", 
           x = c(-0.7, -2.0, 1.1, 0.7), y = c(1.8, -1.2, -0.3, 0.7), label = c("Coffee area?", "Frappuccino area?", "Tea area?", "??"), 
           family = "Sarina", size = 50, color = c("black", "black", "black", "black"), angle = c(11, 9, 3, 5), hjust = 0) + 
  
  scale_size_continuous(name = "Calorie(KCal)", range = c(10, 20), guide = guide_legend(override.aes = list(color = "white"))) + 
  scale_color_gradient(name = "Caffeine(mg)", low = "white", high = "#583822", breaks = c(0, 200, 400)) + 
  scale_alpha_continuous(range = c(0.6, 1.0), guide = "none") + 
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.0, 2.0), clip = "off") + 
  
  labs(title = toupper(Title), caption = Caption) + 
  theme(axis.title = element_text(family = "WSans", face = "bold", size = 110, color = "white"), 
        axis.text = element_text(family = "WSans", size = 110, color = "white"), 
        axis.ticks = element_blank(), 
        legend.background = element_rect(fill = NA), 
        legend.spacing.x = unit(1, "cm"), 
        legend.spacing.y = unit(0.5, "cm"), 
        legend.key =  element_rect(fill = NA), 
        legend.key.width = unit(3, "cm"), 
        legend.text = element_text(family = "WSans", size = 75, color = "white"), 
        legend.text.align = 1, 
        legend.title = element_text(family = "WSans", face = "bold", size = 80, color = "white"), 
        legend.title.align = 0, 
        legend.position = c(1.15, 0.7), 
        legend.box = "vertical",  
        panel.background = element_rect(fill = "#b3814f", color = "white", size = 2), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_blank(), 
        plot.background = element_rect(fill = "#b3814f"), 
        plot.title = element_textbox(family = "WSans", face = "bold", size = 200, color = "white", fill = "#007b43", lineheight = 0.4, margin = margin(-1, 2.5, 1, -2.5, "cm"), padding = margin(1, 0.5, 1, 1, "cm")), 
        plot.caption = element_textbox(family = "WSans", size = 60, color = "white", fill = NA, lineheight = 0.1, margin = margin(0, 1, -0.5, -1, "cm"), padding = margin(0.5, 0.5, 0.5, 0.5, "cm"), hjust = 0), 
        plot.margin = margin(2, 7.7, 1, 1, "cm")
  )


ggsave(result_plot, file = "week52_result_plot.png", width = 15, height = 17)

