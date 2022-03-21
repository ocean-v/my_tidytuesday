# Loading libraries
library(tidyverse)
library(showtext)


# Loading fonts
font_add_google("DM Sans", "DM Sans")
showtext_auto()

font_add_google("Alfa Slab One", "Alfa Slab One")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-03-15')

bioc <- tuesdata$bioc
cran <- tuesdata$cran


# Preparing data
## extracting the date from cran data
cran2 <- cran %>% 
  mutate(
    year = case_when(
      str_detect(date, "UTC$") ~ str_extract(date, pattern = "^[0-9]{4}"), 
      str_detect(date, " [0-9]{4}$") ~ str_extract(date, pattern = "[0-9]{4}$")
    ), 
    month = case_when(
      str_detect(date, "UTC$") ~ str_extract(date, pattern = "(?<=^[0-9]{4}-)[0-9]{2}"), 
      str_detect(date, " [0-9]{4}$") ~ str_extract(date, pattern = "(?<=^[A-Za-z]{3} )[A-Za-z]{3}")
    ), 
    day = case_when(
      str_detect(date, "UTC$") ~ str_extract(date, pattern = "(?<=^[0-9]{4}-[0-9]{2}-)[0-9]{2}"), 
      str_detect(date, " [0-9]{4}$") ~ str_extract(date, pattern = "(?<=^[A-Za-z]{3} [A-Za-z]{3} ).{2}")
    ), 
    hms = case_when(
      str_detect(date, "UTC$") ~ str_extract(date, pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}"), 
      str_detect(date, " [0-9]{4}$") ~ str_extract(date, pattern = "[0-9]{2}:[0-9]{2}:[0-9]{2}")
    )
  ) %>% 
  mutate(
    date2 = case_when(
      str_detect(date, "UTC$") ~ lubridate::ymd_hms(date), 
      str_detect(date, " [0-9]{4}$") ~ paste(day, month, year, hms) %>% lubridate::dmy_hms()
    )
  )

## summarising data
cran3 <- cran2 %>% 
  group_by(package) %>% 
  summarise(n_version_update = n(), dif = as.numeric(max(date2, na.rm = TRUE) - min(date2, na.rm = TRUE)))

## correcting the date of bioc data
bioc2 <- bioc %>% 
  filter(date != lubridate::ymd_hms("1970-01-01 00:00:00")) %>% 
  mutate(date2 = date)

## summarising data
bioc3 <- bioc2 %>% 
  group_by(package) %>% 
  summarise(n_version_update = n(), dif = as.numeric(max(date2, na.rm = TRUE) - min(date2, na.rm = TRUE)))

## binding data
bioc_cran <- bind_rows(
  mutate(cran3, category = "cran"), 
  mutate(bioc3, category = "bioc")
)

result_plot <- ggplot(data = bioc_cran) + 
  geom_point(aes(x = dif, y = n_version_update, color = category), size = 0.3, alpha = 0.5) + 
  scale_x_continuous(limits = c(0, 7500), expand = expansion(mult = c(0.01, 0.00))) + 
  scale_y_log10(limits = c(1, 45000), expand = expansion(mult = c(0.01, 0.00))) + 
  scale_color_manual(values = c("#e60033", "#eb6101"), labels = c("Bioconductor", "CRAN")) + 
  annotate("text", x = 7400, y = 1.5, label = "X: The time between the date of the first upload and the last update (days)", family = "Alfa Slab One", color = "white", size = 8, hjust = 1) + 
  annotate("text", x = 7400, y = 1.2, label = "Y: The frequency of package updates (including the first upload)", family = "Alfa Slab One", color = "white", size = 8, hjust = 1) + 
  labs(
    title = "R Package Update Frequency", 
    caption = "Data comes from  rmflight/vignette_analysis(https://github.com/rmflight/vignette_analysis); Image created by @pat_macro"
  ) + 
  xlab("Time") + 
  ylab("Update frequency") + 
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme(
    axis.title = element_text(family = "Alfa Slab One", color = "white", size = 60), 
    axis.text = element_text(family = "Alfa Slab One", face = "bold", color = "white", size = 45), 
    axis.ticks = element_line(size = 0.5, color = "white"), 
    axis.ticks.length = unit(0.2, "cm"), 
    panel.background = element_rect(fill = "#e8a2a9"), 
    panel.border = element_rect(fill = NA, color = "white"), 
    plot.background = element_rect(fill = "#e87480"), 
    plot.title = element_text(family = "Alfa Slab One", color = "white", size = 100, hjust = 0.85, vjust = 2), 
    plot.caption = element_text(family = "Alfa Slab One", color = "white", size = 15, vjust = -8), 
    plot.margin = margin(0.75, 0.75, 0.5, 0.5, "cm"), 
    legend.position = c(0.68, 0.92), 
    legend.background = element_blank(), 
    legend.title = element_blank(), 
    legend.text = element_text(family = "Alfa Slab One", color = "white", size = 50), 
    legend.direction = "horizontal", 
    legend.key = element_blank()
  )


# Saving  image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 14, unit = "cm", dpi = 400)

