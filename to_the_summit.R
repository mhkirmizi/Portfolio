library(tidyverse)
library(showtext)
library(ggtext)
library(cowplot)
### Fonts
font_add_google("Playfair Display", "playfair")
font_add_google("Lato", "lato")
showtext_auto()
### Text
subtitle <- "Mt. <b>Everest</b>, the highest point on Earth, poses significant challenges to climbers. <span style='color:#94475EFF;'><b>Spring</b></span> stands out as the season to <br> reach the summit, whereas many teams attempted ascents in <span style='color:#364C54FF;'><b>Fall</b></span> but only few managed to reach a little over 8KM. <br>Each dot represents a succesfull climb and and its size reflects the number of climbers in the team."

### Data Wrangling
exped_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv')
exped_tidy |> 
  filter(TOTDAYS != 0) |> 
  filter(HIGHPOINT != 0) |> 
  filter(SEASON_FACTOR %in% c('Autumn', 'Spring')) |> 
ggplot(aes(HIGHPOINT, TOTDAYS, color = SEASON_FACTOR)) +
  geom_point(aes(size = TOTMEMBERS, 
                 shape = factor(TERMREASON == 1, 
                                levels = c(TRUE, FALSE), 
                                labels = c("Success", "Other")))) +
  scale_x_continuous(labels = function(x) paste0(x / 1000, 'KM')) +
  scale_y_continuous(breaks =  seq(10, 50, 10), labels = seq(10, 50,10)) +
  scale_color_manual(values = c('Autumn' = '#364C54FF', 'Spring' = '#94475EFF')) +
  scale_shape_manual(values = c(16, 4))+ 
  labs(x = '', 
       y = 'Days to Summit', 
       title = 'To the Summit: Expeditions to the Roof of the World', 
       subtitle = subtitle, 
       caption = "Data: The Himalayan Database | Vis: MhKirmizi") +
  theme_minimal_hgrid() +
  theme(
    legend.position = "none", 
    text = element_text(size= 14, family = 'lato'),
    plot.title = element_text(size = 42, hjust = .5, 
                              face = 'bold', 
                              family = 'playfair', 
                              color = '#010101FF'
                                ), 
    plot.subtitle = element_textbox(size = 24, hjust = .5, 
                                 family = 'playfair', 
                                 lineheight = 1.25,
                                 halign = .5,
                                 margin = margin(t = 5, b = 5)),
    plot.background = element_rect(color = '#F3F6FB', fill = '#F3F6FB'),
    panel.background = element_rect(color = '#F3F6FB', fill = '#F3F6FB'), 
    panel.grid.major.y = element_line(linetype = 'dashed')
    )
ggsave("to_the_summit.png", width = 1920, height = 1080, units = "px", dpi = 132)
