library(tidyverse)
library(lemon)
library(showtext)
## Fonts
font_add_google("Montserrat", "Montserrat")
showtext_auto()
## Colors 
penguins_hue <- c("#2A2432FF", "#4F3855FF", "#846D86FF","#EFEFCFFF","#D5B77DFF", "#A89E5EFF")
### 
df <- palmerpenguins::penguins
df <- df |> 
  drop_na()

ggplot(df, aes(island,  body_mass_g))+
  geom_point(position = position_jitter(width = .1), 
             aes(color = species), 
             size = 6) +
  lemon::coord_flex_cart(bottom=brackets_horizontal()) +
  scale_color_manual(values = penguins_hue[4:6]) +
  scale_fill_manual(values = penguins_hue[4:6]) +
  labs(
    x = 'Island', 
    y = 'Body Mass',
    title = 'Palmer Penguins', 
    color = 'Species',
    caption = 'data: {palmerpenguins} | Vis: MhKirmizi',
  ) +
  theme_classic(base_family = "Montserrat") +
  theme(
    legend.position = 'top', 
    plot.title = element_text(size = 42, colour = "white", 
                              face = "bold", family = "Montserrat", hjust = 0.5), 
    plot.caption = element_text(size = 14, face = 'bold', color = 'white'),
    plot.background = element_rect(fill = "#1d1330", color = "#1d1330"), 
    panel.background = element_rect(fill = "#1d1330", color = "#1d1330"), 
    axis.text = element_text(color = "white", size = 12, face = 'bold'),
    axis.title = element_text(color = "white", size = 14, face = 'bold'),
    axis.line = element_line(colour = 'white'), 
    legend.background = element_rect(fill = "#1d1330", color = "#1d1330"), 
    legend.text = element_text(size = 12, color = 'white', face = 'bold'), 
    legend.title = element_text(size = 14, color = 'white', face = 'bold')
  )
ggsave("penguins_island.png", width = 1920, height = 1080, units = "px", dpi = 132)
## things to do increase the size of th
