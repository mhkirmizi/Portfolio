library(tidyverse)
library(ggstream)
library(showtext)
### Fonts
font_add_google("Montserrat", 'mont')
font_add_google("Autour One", 'aut')
showtext_auto()

df |> 
  select(-Total) |> 
  pivot_longer(cols = c("Other","Ethnonationalist", "Jihadist", 'Left', 'Right'),
               names_to = 'terror', values_to = 'number') |> 
  mutate(
    terror = case_when(
      terror %in% c('Other', "Ethnonationalist") ~ 'Other', 
      TRUE ~ terror
    ), 
    terror = factor(terror)
  ) |> 
  group_by(Year, terror) |> 
  summarise(number = sum(number), .groups = 'drop') |> 
  ggplot(aes(Year, number, fill = terror, label = terror, color = terror)) +
  geom_stream(extra_span = 0.013, type = "mirror", n_grid = 3000, bw = .78) +
  geom_stream_label(size =6, type = 'mirror', n_grid = 1000) +
  scale_colour_manual(
    values = paletteer::paletteer_d("LaCroixColoR::Coconut")
    %>% colorspace::darken(.8)) +
  scale_fill_manual(
    values = paletteer::paletteer_d("LaCroixColoR::Coconut")
    %>% colorspace::lighten(.2)) +
  labs(x = '', 
       y = '', 
       title = 'Terrorism and Political Violence in the USA',
       caption = 'Data: CSIS | Vis: MhKirmizi') +
    annotate("text", x = 2004, y = 15, 
             label = "September, 11", vjust = 1, color= "grey66") +
    geom_curve(
      aes(x = 2004, y = 13.5, xend = 2001, yend = 9),
      arrow = arrow(length = unit(0.2, "in")),
      curvature = - 0.3,
      color = "gray66"
    ) +
  cowplot::theme_minimal_vgrid(font_size = 18, font_family = 'aut') +
  theme(legend.position = 'none', 
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 28, hjust = .5, 
                                  family = 'mont', face = 'bold'), 
        plot.background = element_rect(colour = 'white', fill = 'white'), 
        panel.background = element_rect(colour = 'white', fill = 'white'))
ggsave("week45.png", width = 1920, height = 1080, units = "px", dpi = 132)  


