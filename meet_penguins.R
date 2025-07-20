library(tidyverse)
library(showtext)
library(extrafont)
library(ggtext)

## Data
penguins <- palmerpenguins::penguins
penguins <-  penguins |> 
  filter(!is.na(flipper_length_mm), 
         !is.na(bill_length_mm), 
         !is.na(sex))
## Fonts
font_add_google("Montserrat", "Montserrat")
showtext_auto()
## Text
title <- "Meet the Palmer Penguins"
subtitle <- "Palmer penguins is a very popular dataset in the data science and statistics community. Here, you can see the distribution of three species: 
<br> <span style='color:#D5D5D3;'>Adélie,</span> <span style='color:#C27D38;'>Chinstrap,</span> and <span style='color:#CEAB07;'>Gentoo</span>. 
with respect to two variables: Bill Length and Flipper Lenth. The size of each dot represents the body mass. 
<br> Gentoos seem to be larger than others. Chinstrap tend to have a larger flipper and bill than Adélie; however, they have a similar body size."
caption <- "Data: Palmer Penguins | Vis: MhKirmizi"

## Colors
penguins_hue <- c("#D5D5D3", "#C27D38","#CEAB07","#F3DF6C", "#24281A", "#798E87")

ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = species)) +
  geom_point(aes(size = body_mass_g)) +
  ggside::geom_xsidedensity(aes(fill = species), alpha = .6) +
  scale_color_manual(values = penguins_hue[1:3]) +
  scale_fill_manual(values = penguins_hue[1:3]) +
  labs(x = "Bill Length", 
       y = "Flipper Length", 
       title = title, 
       subtitle = subtitle,
       caption = caption) +
  theme_minimal(base_family = "Montserrat") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = penguins_hue[5], color = penguins_hue[5]), 
        panel.background = element_rect(fill = penguins_hue[5], color = penguins_hue[5]), 
        plot.title = element_text(size = 36, colour = penguins_hue[6], 
                                  face = "bold", family = "Montserrat", hjust = 0.5),
        plot.subtitle = element_markdown(size = 18, family = "Montserrat",hjust = .5 , 
                                         color = penguins_hue[6]),
        plot.caption = element_text(size = 12, color = penguins_hue[6]),
        panel.grid = element_line(color = penguins_hue[5]), 
        axis.title = element_text(size = 12, color = penguins_hue[6]),
        axis.ticks = element_blank(), 
        axis.text = element_blank())
ggsave("meet_penguins.png", width = 1920, height = 1080, units = "px", dpi = 132)

