library(tidyverse)
library(mandelbrot)
library(pbapply)
library(parallel)

# Define parameters for iterations ---------------------------------------------
x_start = -0.1; x_end = -1.0406505
y_start = 0; y_end = 0.348484
strt_dim = 4; end_dim = 1E-6

n = 30*10 # Number of images/frames

res = 1600L # mandelbrot image resolution
itr = 100

img_res = 800 # Output image resolution

sigmoid = function(x) { # Use sigmoid to scale "speed" of zoom
  exp(x)/(exp(x) + 1)
}

# The range is smaller on the positive side to account for the smaller step
sig_seq = sigmoid(seq(-5, 20, length.out = n))  

# ------------------------------------------------------------------------------

# Create a temporay directory
dir.create("IMAGES", recursive = T, showWarnings = F)

pboptions(type = "timer")

x_coordinates = x_start + (x_end - x_start)*sig_seq
y_coordinates = y_start + (y_end - y_start)*sig_seq
box_dims = strt_dim + (end_dim - strt_dim)*sig_seq

params = matrix(
  rep(
    c(
      x_coordinates - box_dims/2, 
      x_coordinates + box_dims/2,
      y_coordinates - box_dims/2, 
      y_coordinates + box_dims/2
      )
    ),
  nrow = n
) 

cl = makeForkCluster(detectCores())

pblapply(cl = cl, 1:nrow(params), function(i) {
  r = params[i,] %>% as.numeric()
  
  fn = paste0("IMAGES/ts", sprintf("%04d", i), ".png")
  
  png(file = fn, width = img_res, height = img_res)
  
  tb = mandelbrot(
    xlim = r[1:2],
    ylim = r[3:4],
    resolution = res,
    iterations = itr
  ) %>% as.data.frame()
  
  p = tb %>% 
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
