# Loading libraries
library(tidyverse)
library(showtext)
library(ggh4x)
library(countrycode)


# Loading font
font_add_google("Roboto", "Roboto")
font_add_google("Dancing Script", "Dancing Script")

showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-01-18')

chocolate <- tuesdata$chocolate


# Preparing data for plot
chocolate2 <- chocolate %>% 
  mutate(No._of_shops = 1) %>% 
  mutate(cocoa_percent2 = as.numeric(str_extract(cocoa_percent, "[0-9]+")) * 0.01) %>% 
  mutate(
    B = as.numeric(str_detect(ingredients, "B,|B$")), 
    S = as.numeric(str_detect(ingredients, "S,|S$")), 
    So = as.numeric(str_detect(ingredients, "S[^,a]{1},|S[^,a]{1}$")), 
    C = as.numeric(str_detect(ingredients, "C,|B$")), 
    V = as.numeric(str_detect(ingredients, "V,|V$")), 
    L = as.numeric(str_detect(ingredients, "L,|L$")), 
    Sa = as.numeric(str_detect(ingredients, "Sa,|Sa$"))
  ) %>% 
  mutate_at(vars(12:19), replace_na, 0)

chocolate3 <- chocolate2 %>% 
  group_by(company_location) %>% 
  summarise_at(vars(c("No._of_shops", "B", "S", "So", "C", "V", "L", "Sa")), sum) %>% 
  pivot_longer(cols = c("B", "S", "So", "C", "V", "L", "Sa"), names_to = "ingredient_name", values_to = "count") %>% 
  mutate(ingredient_name = factor(ingredient_name, levels = c("B", "S", "So", "C", "V", "L", "Sa"))) %>% 
  mutate(company_location_3c = case_when(
    company_location == "Amsterdam" ~ "AMS", 
    company_location == "Scotland" ~ "SCT", 
    company_location == "Wales" ~ "WLS", 
    TRUE ~ countrycode(company_location, origin = "country.name", destination = "iso3c")
    )
  ) %>% 
  mutate(strip_legend = paste0(company_location_3c, " (", No._of_shops , ")"))

result_plot <- ggplot(data = chocolate3) + 
  geom_bar(aes(x = ingredient_name, y = count, fill = ingredient_name), color = "black", size = 0.1, stat = "identity") + 
  scale_fill_discrete(labels = c("Beans", "Sugar", "Other sweetener", "Cocoa Butter", "Vanilla", "Lecithin", "Salt")) + 
  facet_wrap2(~strip_legend, scales = "free_y", ncol = 10) + 
  labs(
    title = "Rough characteristics of each country's chocolate", 
    subtitle = "Counts of each ingredient contained in rated chocolates made by shops in each country", 
    caption = "*The number represents the number of rated chocolates       Data derived from Flavors of Cacao (http://flavorsofcacao.com/chocolate_database.html); Image created by @pat_macro"
  ) + 
  theme(
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    legend.background = element_blank(), 
    legend.key.size = unit(0.22, "cm"), 
    legend.box.background = element_blank(), 
    legend.title = element_blank(), 
    legend.text = element_text(family = "Roboto", face = "bold", size = 14, color = "white"), 
    legend.position = c(0.8, 0.05), 
    legend.direction = "horizontal", 
    panel.background = element_rect(fill = "#bd8b77"), 
    panel.border = element_rect(fill = NA, color = "black", size = 0.1), 
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = "#9e5d42"), 
    plot.title = element_text(family = "Dancing Script", face = "bold", size = 65, color = "white"), 
    plot.subtitle = element_text(family = "Dancing Script", size = 35, color = "white"), 
    plot.caption = element_text(family = "Roboto", size = 15, color = "white"), 
    strip.background = element_blank(), 
    strip.text = element_text(family = "Roboto", face = "bold", size = 18, color = "white")
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 12, unit = "cm", dpi = 300)

