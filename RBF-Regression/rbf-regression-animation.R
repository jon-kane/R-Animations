library(tidyverse)
library(gganimate)

data_list = readRDS('data-list.rds')

# Animation options
res = 100; w = h = 500; fps = 30
n_frames = data_list$tb_pred %>%
  pull(type) %>% 
  unique() %>% 
  length()

# ------------------------------------------------------------------------------
anim = ggplot() + 
  geom_line(data = data_list$tb_vis, aes(x = x, y = y, color = "true")) + 
  geom_point(data = data_list$tb_train, aes(x, y, color = "train point")) + 
  geom_line(data = data_list$tb_pred, aes(x, y, color = "model"), alpha = 0.8) + 
  scale_color_manual(
    values = c("true" = "red", "train point" = "blue", "model" = "black")
  ) + 
  labs(
    color = "color",
    title = "reg. parameter = {current_frame}"
  ) + 
  theme(legend.position = "top") + 
  ylim(-2, 6) + 
  transition_manual(frames = type)

anim_save(
  'rbf-regression.gif', 
  anim,
  width = w,
  height = h, 
  res = res,
  nframes = n_frames*2,
  fps = fps,
  rewind = TRUE
)
