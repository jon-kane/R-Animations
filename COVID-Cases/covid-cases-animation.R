library(tidyverse)
library(fiftystater) # All 50 states together
library(lubridate)
library(gganimate)
library(scales) # pseudo_log_trans

# Data link (CDC): https://bit.ly/2MVWWqZ
tb_covid_raw = read_csv('cdc-covid-cases.csv') 

# Data link (Census): https://bit.ly/3jgN7jg
tb_pop_raw = read_csv('nst-est2020.csv') 

state_nm_abb = tibble(
  state_name = state.name,
  state_abb = state.abb 
)

# Join the state name with state abbreviation for population stats ------------- 
tb_pop_cleaned = tb_pop_raw %>% 
  select(NAME, POPESTIMATE2020) %>% 
  rename(state_name = NAME) %>% 
  left_join(state_nm_abb, by = 'state_name') %>% 
  drop_na()

# Join tb_pop_edit to tb_covid_edit --------------------------------------------
tb_covid_cleaned = tb_covid_raw %>% 
  rename(state_abb = state) %>% 
  left_join(tb_pop_cleaned, by = 'state_abb') %>% 
  mutate(
    submission_date = as_date(submission_date, format = '%m/%d/%Y')
  ) %>% 
  mutate(state_name = tolower(state_name)) %>% 
  mutate(`Cases Per 100K` = (tot_cases/POPESTIMATE2020)*10^5) %>% 
  select(submission_date, state_name, new_case, `Cases Per 100K`)

# See here for per 100K explanation: https://bit.ly/3oIjLvd
# Cases per 100K ---------------------------------------------------------------
map_base_plot = tb_covid_cleaned %>%
  ggplot() +
  geom_map(
    aes(
      map_id = state_name, 
      fill = `Cases Per 100K`
    ), 
    map = fifty_states
  ) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_viridis_c(
    option = 'magma', 
    trans = pseudo_log_trans(base = 10), # smoothly transition to linear scale around 0
    breaks = 10^(1:7)
  ) + 
  theme_void() + 
  fifty_states_inset_boxes()

map_anim = map_base_plot + 
  labs(title = 'Date: {current_frame}') + # current_frame variable comes from transition manual
  transition_manual(frames = submission_date) # transition_states tries to interpolate

num_frames = tb_covid_cleaned$submission_date %>% 
  unique() %>% 
  length()

anim_save(
  'covid-map.gif',
  map_anim,
  width = 700, 
  height = 700, 
  res = 150, 
  fps = 10, 
  nframes = num_frames
)

# Number of cases per day ------------------------------------------------------
daily_cases_anim = tb_covid_cleaned %>% 
  group_by(submission_date) %>% 
  summarize(cases = sum(new_case)) %>% 
  ggplot(aes(x = submission_date, y = cases)) +
  geom_point() + 
  geom_line() + 
  labs(x = 'date', title = 'Cases Nationwide Per Day') + 
  theme_classic() + 
  transition_reveal(along = submission_date) 

anim_save(
  'daily-cases.gif',
  daily_cases_anim,
  width = 700, 
  height = 700, 
  res = 150, 
  fps = 10, 
  nframes = num_frames
)

# Join the gifs together -------------------------------------------------------
library(magick) # Wrapper for ImageMagick

map_gif = image_read('covid-map.gif')
daily_gif = image_read('daily-cases.gif')

lapply(1:length(map_gif), function(i) {
  image_append(c(map_gif[i], daily_gif[i]))
}) %>% 
  image_join() %>% 
  image_animate(fps = 10, dispose = 'none') %>% 
  image_write(path = 'map-and-daily-joined.gif')
