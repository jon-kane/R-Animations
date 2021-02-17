library(tidyverse)
library(mandelbrot)
library(pbapply)
library(parallel)
library(magick)

# Define parameters for iterations ---------------------------------------------
x_start = -0.1; x_end = -1.0406505
y_start = 0; y_end = 0.348484
strt_dim = 4; end_dim = 1E-6

n = 250 # Number of images/frames

sigmoid = function(x) { # Use sigmoid to scale "speed" of zoom
  exp(x)/(exp(x) + 1)
}

# The range is smaller on the positive side to account for the smaller step
sig_seq = sigmoid(seq(-5, 20, length.out = n))  

# ------------------------------------------------------------------------------
x_coordinates = x_start + (x_end - x_start)*sig_seq
y_coordinates = y_start + (y_end - y_start)*sig_seq
box_dims = strt_dim + (end_dim - strt_dim)*sig_seq

params = matrix(
  c(
    x_coordinates - box_dims/2, 
    x_coordinates + box_dims/2,
    y_coordinates - box_dims/2, 
    y_coordinates + box_dims/2
  ),
  nrow = n
) 

# Create a temporary directory
dir.create("IMAGES", recursive = T, showWarnings = F)
pboptions(type = "timer")
cl = makeForkCluster(detectCores())
pblapply(cl = cl, 1:nrow(params), function(i) {
  r = params[i,] %>% as.numeric()

  df = mandelbrot(
    xlim = r[1:2],
    ylim = r[3:4],
    resolution = 1000,
    iterations = 100
  ) %>% as.data.frame()

  fn = paste0("IMAGES/ts", sprintf("%04d", i), ".png")

  png(file = fn, width = 700, height = 700)

  p = df %>%
    ggplot(aes(x, y, fill = value)) +
    geom_raster(interpolate = TRUE) +
    theme_void() +
    scale_fill_viridis_c(option = "A", guide = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  print(p)

  invisible(dev.off())
}) %>% invisible()
stopCluster(cl)

imgs = list.files("IMAGES", full.names = T)

pblapply(c(imgs, rev(imgs)), image_read) %>% 
  image_join() %>% 
  image_animate(fps = 25, dispose = "none") %>% 
  image_write(path = "mandelbrot.gif")

unlink("IMAGES", recursive = T)