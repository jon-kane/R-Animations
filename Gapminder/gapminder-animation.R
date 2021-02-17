library(tidyverse)
library(gganimate)
library(gapminder)

anim = gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = continent_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(
    title = 'Year: {frame_time}', 
    x = 'GDP per capita', 
    y = 'life expectancy'
    ) +
  transition_time(year) + 
  ease_aes('linear')

anim_save(
  'gapminder-gganimate.gif', 
  anim,
  width = 700,
  height = 700, 
  res = 150,      
  nframes = 250,
  fps = 25,
  renderer = gifski_renderer()
)
