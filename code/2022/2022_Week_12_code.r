# Loading libraries
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)


# Loading fonts
font_add_google("Pacifico", "Pacifico")
showtext_auto()


# Loading data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

us_births <- tuesdata$applicants
us_babynames <- tuesdata$babynames

nz_births <- tuesdata$nz_births
nz_babynames <- tuesdata$nz_names


# Preparing data
## Reshaping nzdata
nz_births2 <- nz_births %>% 
  select(- Total) %>% 
  pivot_longer(cols = c(Male, Female), names_to = "sex", values_to = "n_all") %>% 
  mutate(
    sex = case_when(
      sex == "Male" ~ "M", 
      sex == "Female" ~ "F"
    )
  ) %>% 
  rename(year = Year) %>% 
  drop_na()

nz_babynames2 <- nz_babynames %>% 
  rename(year = Year, sex = Sex, name = Name, n = Count) %>% 
  mutate(
    sex = case_when(
      sex == "Male" ~ "M", 
      sex == "Female" ~ "F"
    )
  )


## Reshaping data for plot
reshape_babyname <- function(babynames, births) {
  babynames %>% 
    group_by(year, sex) %>% 
    nest() %>% 
    mutate(
      No1_name = map_chr(data, function(data) {
          no1_name <- data %>% 
            filter(n == max(n)) %>% 
            pull(name)
          
          return(no1_name)
        }
      )
    )
  
  no1_names <- babynames %>% 
    group_by(year, sex) %>% 
    filter(n == max(n)) %>% 
    ungroup() %>% 
    rename(n_no1 = n) %>% 
    right_join(births, by = c("year", "sex")) %>% 
    mutate(n_others = n_all - n_no1) %>% 
    pivot_longer(cols = c(n_no1, n_others), names_to = "Category", values_to = "n") %>% 
    mutate(
      Name = case_when(
        Category == "n_no1" ~ name, 
        Category == "n_others" ~ "X_Others"
      )
    ) %>% 
    select(year, sex, Name, Category, n)
    
    return(no1_names)
}


no1_names_US <- reshape_babyname(babynames = us_babynames, births = us_births)
no1_names_NZ <- reshape_babyname(babynames = nz_babynames2, births = nz_births2)

no1_names <- list(US = no1_names_US, NZ = no1_names_NZ)


# Creating plots
for (ii in list(M = list("US", "M", "Male", "VanGogh2", c(0, 2300000)), F = list("US", "F", "Female", "Cross", c(0, 2300000)), M_nz = list("NZ", "M", "Male", "Hokusai1", c(0, 35000)), F_nz = list("NZ", "F", "Female", "Juarez", c(0, 35000)))) {
  no1_names_tmp <- no1_names[[ii[[1]]]] %>% filter(sex == ii[[2]])

  number_of_names_tmp <- no1_names_tmp %>% count(Name) %>% pull(Name)
  
  g <- ggplot(data = no1_names_tmp) + 
    geom_bar(aes(x = year, y = n, fill = Name), stat = "identity") + 
    scale_y_continuous(limits = ii[[5]]) + 
    scale_fill_manual(
      values = c(MetBrewer::met.brewer(name = ii[[4]], n = length(number_of_names_tmp) - 1), "#d6c6af"), 
      labels = c(number_of_names_tmp[number_of_names_tmp != "X_Others"], "Others")
    ) + 
    guides(fill = guide_legend(ncol = 2)) + 
    labs(title = paste0(ii[[1]], "-", ii[[3]])) + 
    theme(
      axis.title = element_blank(), 
      axis.text = element_text(family = "Pacifico", color = "black", size = 15), 
      axis.line.x = element_line(color = "black"), 
      axis.line.y = element_line(color = "black"), 
      panel.background = element_rect(fill = "#ede4cd", color = "#ede4cd"), 
      panel.grid = element_blank(), 
      plot.background = element_rect(fill = "#ede4cd", color = "#ede4cd"), 
      plot.title = element_text(family = "Pacifico", color = "black", size = 15), 
      legend.background = element_blank(), 
      legend.position = c(0.95, 0.03),  
      legend.title = element_blank(), 
      legend.text = element_text(family = "Pacifico", color = "black", size = 10), 
      legend.key.size = unit(0.25, "cm"), 
      legend.justification = c(1, 0)
    )
  
  if (ii[[2]] == "M") {
    g <- g + theme(
      axis.text.x = element_blank(), 
      axis.line.x = element_blank(), 
      axis.ticks.x = element_blank()
    )
  }

  assign(x = paste0("plot_", ii[[1]], "_", ii[[2]]), value = g)
}


design <- c("
AC
BD
")

result_plot <- plot_US_M + plot_US_F + plot_NZ_M + plot_NZ_F + 
  plot_layout(design = design) + 
  plot_annotation(
    title = "<span style=color:'#47885e'>The Most Popular Baby Names</span> in the U.S. and New Zealand", 
    subtitle = "Showing most popular names by colors and total baby births by bars", 
    caption = "Data comes from  hadley/babynames(https://github.com/hadley/babynames) & ekothe/nzbabynames(https://github.com/ekothe/nzbabynames); Image created by @pat_macro", 
    theme = theme(
      plot.background = element_rect(fill = "#ede4cd"), 
      plot.title = element_textbox(family = "Pacifico", size = 30, hjust = 0.5), 
      plot.subtitle = element_text(family = "Pacifico", color = "#c9171e", size = 18, hjust = 0.5), 
      plot.caption = element_text(family = "sans", size = 8)
    )
  )


# Saving image
ggsave(result_plot, file = "result_plot.png", width = 14, height = 14, units = "cm", dpi = 200)

