library(tidyverse)
library(rvest)
library(lubridate)
library(paletteer)
library(showtext)
library(ggtext)
library(ggh4x)
library(hrbrthemes)

## Colors


## Texts
title <- "Change in Length of Day in Helsinki And Wellington"
subtitle <- "The diagram below illustrates how the length of daylight changes throughout the year.Helsinki, one of the northernmost capitals, \nand Wellington, the capital of New Zealand, display opposite seasonal patterns.As days grow longer in the Northern Hemisphere, \nthey shorten in the Southern Hemisphere, and vice versa. The Summer Solstice in the Northern Hemisphere, marking the longest day of the year, \ncoincides with the Winter Solstice in the Southern Hemisphere, marking the shortest day, and the reverse occurs six months later."


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

dl_helsinki |> 
  left_join(dl_wellington, by = c("Date", "date")) |> 
  mutate(length.x = length.x / 3600, 
         length.y = length.y / 3600) |> 
  ggplot(aes(x = date)) +
  stat_difference(aes(ymin = length.y, ymax = length.x), alpha = .3)+
  geom_line(aes(y = length.x, colour = "max")) +
  geom_line(aes(y = length.y, colour = "min")) +
  annotate("text", x =  as.Date("2024-03-1"), y = 14, label = "Spring Equinox", 
           vjust = -1, size = 5)+
  annotate("text", x =  as.Date("2024-10-15"), y = 14, label = "Fall Equinox", 
           vjust = -1, size = 5) +
  geom_textbox(data = data.frame(x = as.Date("2024-06-20"), y = 21), 
               aes(x = x, y = y), 
               label = "☀️ Summer Solstice: Longest day in Helsinki", 
               box.color = "black", width = unit(3, "in"), 
               halign = 0.5, size = 5) + 
  geom_textbox(data = data.frame(x = as.Date("2024-06-20"), y = 7), 
               aes(x = x, y = y), 
               label = "☀️ Winter Solstice: Shortest day in Wellington", 
               box.color = "black", width = unit(3, "in"), 
               halign = 0.5, size = 5) +
  annotate("curve", x = as.Date("2024-03-5"), y = 14.5, xend = as.Date("2024-03-20"), 
           yend = 12.5, curvature = - 0.2, arrow = arrow(length = unit(0.1, "inches")), 
           color = "black") +
  annotate("curve", x = as.Date("2024-10-5"), y = 14.5, xend = as.Date("2024-9-23"), 
           yend = 12.5, curvature = 0.2, arrow = arrow(length = unit(0.1, "inches")), 
           color = "black") + 
  scale_y_continuous(breaks = seq(0,24,3), labels = seq(0, 24, 3), limits = c(0, 24)) + 
  labs(
    x = "", 
    y = "Day Length", 
    title = title, 
    subtitle = subtitle, 
    caption = "Data: sunrise-and-sunset.com | Data: MhKirmizi",
  ) + 
  theme_ipsum_gs() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 32, face= "bold", 
                              hjust = .5),
    plot.subtitle = element_text(size = 15, hjust = .5),
    plot.caption = element_text(size = 11),
    legend.position = "none", 
    plot.background = element_rect(fill = "white")
  )
ggsave("daylength.png", dpi = 360, width = 15, height = 12)
