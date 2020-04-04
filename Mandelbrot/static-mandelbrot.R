library(tidyverse)
library(mandelbrot)

res = 1080

png(file = "static-mandelbrot.png", width = res, height = res)

p = mandelbrot(
  xlim = c(-2, 1.4),
  ylim = c(-1.7, 1.7),
  resolution = 2000,
  iterations = 100
) %>% as.data.frame() %>% 
  ggplot(aes(x, y, fill = value)) + 
  geom_raster(interpolate = TRUE) + 
  theme_void() +
  scale_fill_viridis_c(option = "A", guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

print(p)

invisible(dev.off())

