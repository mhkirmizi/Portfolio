library(tidyverse)
library(rvest)
library(lubridate)
library(paletteer)
library(showtext)
library(ggtext)
## data Wrangling
daylight_helsinki <- read_html("https://www.sunrise-and-sunset.com/en/sun/finland/helsinki/2024") 
process_daylight_helsinki <- function(daylight_helsinki, index) {
  daylight_data <- daylight_helsinki %>% 
    rvest::html_table(fill = TRUE) %>%
    .[[index]] %>%
    setNames(c("Date", "Sunrise", "Sunset", "Day_length")) %>% 
    select(c("Date", "Sunrise", "Sunset", "Day_length")) %>%
    slice(-1) %>% 
    mutate(
      Date = str_remove(Date, ",.*"),
      date = as.Date(Date, format = "%d %B %Y"), 
      length = as.numeric(hm(`Day_length`))
    )
  return(daylight_data)
}

dl_helsinki <- lapply(1:12, function(i) process_daylight_helsinki(daylight_helsinki, i)) %>%
    bind_rows()

dl_helsinki <- dl_helsinki |> 
  mutate(city = "Helsinki")


daylight_singapore <- read_html("https://www.sunrise-and-sunset.com/en/sun/singapore/singapore/2024") 
process_daylight_singapore <- function(daylight_singapore, index) {
  daylight_data <- daylight_singapore %>% 
    rvest::html_table(fill = TRUE) %>%
    .[[index]] %>%
    setNames(c("Date", "Sunrise", "Sunset", "Day_length")) %>% 
    select(c("Date", "Sunrise", "Sunset", "Day_length")) %>%
    slice(-1) %>% 
    mutate(
      Date = str_remove(Date, ",.*"),
      date = as.Date(Date, format = "%d %B %Y"), 
      length = as.numeric(hm(`Day_length`))
    )
  return(daylight_data)
}

dl_singapore <- lapply(1:12, function(i) process_daylight_singapore(daylight_singapore, i)) %>%
  bind_rows()
dl_singapore <- dl_singapore |> 
  mutate(city = "Singapore")

daylight_wellington <- read_html("https://www.sunrise-and-sunset.com/en/sun/new-zealand/wellington/2024") 
process_daylight_wellington <- function(daylight_wellington, index) {
  daylight_data <- daylight_wellington %>% 
    rvest::html_table(fill = TRUE) %>%
    .[[index]] %>%
    setNames(c("Date", "Sunrise", "Sunset", "Day_length")) %>% 
    select(c("Date", "Sunrise", "Sunset", "Day_length")) %>%
    slice(-1) %>% 
    mutate(
      Date = str_remove(Date, ",.*"),
      date = as.Date(Date, format = "%d %B %Y"), 
      length = as.numeric(hm(`Day_length`))
    )
  return(daylight_data)
}

dl_wellington <- lapply(1:12, function(i) process_daylight_wellington(daylight_wellington, i)) %>%
  bind_rows()

dl_wellington <- dl_wellington |> 
  mutate(city = "Wellington")

df <- bind_rows(dl_helsinki, dl_singapore, dl_wellington)


## colors
paletteer_d("nationalparkcolors::BlueRidgePkwy")
colors <- c( "#C969A1FF", "#CE4441FF", "#EE8577FF", "#EB7926FF", "#FFBB44FF", 
             "#859B6CFF", "#62929AFF", "#004F63FF", "#122451FF" )
         

## Fonts
font_add_google("Open Sans")

## Text
title <- "Daylight Patterns in Three Cities: An Extension of Cara Tomphson’s Work"
subtitle <- "The length of the day changes based on a location's position on the globe. In  <span style='color:#EB7926FF;'>**Singapore**</span>, located near the Equator, daylight duration 
<br>remains nearly constant throughout the year, creating an almost perfect cycle. In contrast, <span style='color:#EE8577FF;'>**Helsinki**</span> experiences less than six hours of daylight 
<br>in January and very long days in July, reflecting a highly uneven distribution. Meanwhile, <span style='color: #FFBB44FF;'>**Wellington**</span>, the capital of New Zealand and 
<br> one of the southernmost cities, follows an opposite seasonal pattern, with extended daylight hours in January and shorter days in July."
caption <- "Data: Sunrise-and-Sunset.com |  Vis: MhKirmizi"
## PLot 

ggplot(df, aes(x= date, y = length, fill = city)) +
  geom_col(linewidth =.2, alpha = .7) + 
  ylim(-25000, 70000) +
  scale_fill_manual(values = colors[3:5]) +
  coord_polar()+
  facet_wrap(~city) +
  labs(
    title = title, 
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void(base_size = 12, base_family = "Open Sans") +
  theme(
   plot.background = element_rect(colour = colors[9], fill = colors[[9]]),
  panel.background = element_rect(colour = colors[9], fill = colors[[9]]),
    plot.title = element_markdown(size = 28, face = "bold", hjust = .5, color = colors[5]),
    plot.subtitle = element_markdown(size = 16, color = colors[1], hjust = .5), 
    plot.caption = element_text(size = 12, colour = colors[1], hjus= .5),
    axis.title = element_blank(),
    axis.text.y = element_blank(), 
    legend.position = "none",
    strip.text = element_blank()
  )
  
ggsave("daylentgh.png", width = 1920, height = 1080, units = "px", dpi = 132)
