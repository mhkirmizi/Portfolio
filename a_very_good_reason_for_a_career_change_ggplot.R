library(tidyverse)
library(showtext)
library(ggrain)
library(scales)
### Fonts
font_add_google('Outfit', 'Outfit')
showtext_auto()
### Text
title <- "A Very Good Reason for a Career Change"
subtitle <- "With Data Science rapidly growing in popularity, many are choosing it as a new career path for a promising future. \nThe chart highlights that entry-level salaries approach $100K, and earnings rise steadily with each promotion."
## Colors
hue <- c("Entry" = "#4A7169FF", "Middle" =  "#BEB59CFF", "Senior" = "#735231FF", "Executive" = "#49271BFF")
###
df <- read_csv("D:/ML/Kaggle/DS_Salaries/salaries.csv")
df <- df |> 
  filter(work_year == '2025' & job_title == "Data Scientist" & company_location == "US")

unique(df$experience_level)

df <- df |> 
  mutate(experience_level= factor(recode(experience_level, 
                "EN" = "Entry", 
                "MI" = "Middle", 
                "SE" = "Senior", 
                "EX" = "Executive"), 
         levels = c("Entry", "Middle", "Senior", "Executive"), 
         ordered = TRUE))

ggplot(df, aes(experience_level, salary_in_usd, fill = experience_level)) + 
  geom_rain(alpha = .6) +
  geom_hline(yintercept =  100000, linetype = 'dashed', color = 'steelblue') +
  scale_fill_manual(values = hue)+
  scale_y_continuous(labels = label_currency(prefix = "$", 
                                           scale_cut = cut_long_scale(space = FALSE))) +
  annotate("text", x = .65, y = 220000, 
           label = "Six Figures Line", vjust = 1, color= "grey66") +
  geom_curve(
    aes(x = .65, y = 195000, xend = .65, yend = 110000),
    arrow = arrow(length = unit(0.2, "in")),
    curvature = 0.3,
    color = "gray66"
  ) +
  labs(y = "", 
       x = "", 
       title = title,
       subtitle = subtitle,
       caption = "Data: Kaggle | Vis:MhKirmizi") +
  theme_minimal(base_family = "Outfit", base_size = 16) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 48, hjust = .5, family = "Outfit", face = "bold"), 
    plot.subtitle = element_text(size = 24, hjust = .5), 
    plot.background = element_rect(fill = "white", colour = "white"), 
    panel.background = element_rect(fill = "white", colour = "white")
  )
ggsave("a_very_good_reason_for_a_career_change_ggplot.png", width = 1920, height = 1080, units = "px", dpi = 132)  
  
