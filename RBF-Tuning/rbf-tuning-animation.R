library(tidyverse)
library(pbapply)
library(lattice)
library(magick)
library(latex2exp)
library(grid)

data_list = readRDS("data-list.rds")

# Get plot limits
xlim = range(data_list$X_vis$x)
ylim = range(data_list$X_vis$y)
zlim = range(data_list$X_vis$z)

# Panel function for wireframe -------------------------------------------------
myPanel = function(
  x, y, z,
  xlim, ylim, zlim,
  xlim.scaled, ylim.scaled, zlim.scaled,
  pts,
  ...
) {
  panel.3dwire(
    x = x, y = y, z = z,
    xlim = xlim,
    ylim = ylim,
    zlim = zlim,
    xlim.scaled = xlim.scaled,
    ylim.scaled = ylim.scaled,
    zlim.scaled = zlim.scaled,
    ...
  )
  xx = xlim.scaled[1] + diff(xlim.scaled) *
    (pts$x - xlim[1]) / diff(xlim)
  yy = ylim.scaled[1] + diff(ylim.scaled) *
    (pts$y - ylim[1]) / diff(ylim)
  zz = zlim.scaled[1] + diff(zlim.scaled) *
    (pts$z - zlim[1]) / diff(zlim)
  panel.3dscatter(
    x = xx,
    y = yy,
    z = zz,
    xlim = xlim,
    ylim = ylim,
    zlim = zlim,
    xlim.scaled = xlim.scaled,
    ylim.scaled = ylim.scaled,
    zlim.scaled = zlim.scaled,
    col = "red",
    pch = 19,
    ...
  )
}

# Get the static plot ----------------------------------------------------------
png(file = "static-plot.png", width = 700, height = 700)

trellis.par.set("box.3d", list(col = "transparent"))
trellis.par.set("axis.line", list(col = "black"))
trellis.par.set("par.xlab.text", list(col = "black", cex = 2))
trellis.par.set("par.ylab.text", list(col = "black", cex = 2))
trellis.par.set("par.zlab.text", list(col = "black", cex = 2, rot = 90))

p = wireframe(
  z ~ x * y, data_list$X_vis, distance = 0, screen = list(z = 30, x = -70),
  pts = data_list$X_train, panel.3d.wireframe = myPanel,
  xlim = xlim, ylim = ylim, zlim = zlim,
  zlab = "f(x, y)"
)

print(p)

invisible(dev.off())

# Create images for gif --------------------------------------------------------

pboptions(type = "timer")

dir.create("IMAGES", recursive = T, showWarnings = F)

pblapply(1:data_list$n_frames, FUN = function(i) {
  tmp = data_list$X_mod %>% 
    filter(index == i)
  
  fn = paste0("IMAGES/ts", sprintf("%04d", i), ".png")
  
  png(file = fn, width = 700, height = 700)
  
  ttl = paste0("c = ", tmp$c %>% first())
  
  trellis.par.set("box.3d", list(col = "transparent"))
  trellis.par.set("axis.line", list(col = "black"))
  trellis.par.set("par.xlab.text", list(col = "black", cex = 2))
  trellis.par.set("par.ylab.text", list(col = "black", cex = 2))
  trellis.par.set("par.zlab.text", list(col = "black", cex = 2, rot = 90))
  
  p = wireframe(
    z_hat ~ x * y, tmp, distance = 0, screen = list(z = 30, x = -70),
    pts = data_list$X_train, panel.3d.wireframe = myPanel,
    xlim = xlim, ylim = ylim, zlim = zlim,
    zlab = TeX("\\hat{f}(x, y)")
  )
  
  print(p)
  
  grid.text(ttl, x=unit(0.3, "npc"), y=unit(0.8, "npc"), gp = gpar(fontsize = 28))
  
  invisible(dev.off())
}) %>% invisible()

# Join the images using magick -------------------------------------------------
imgs = list.files("IMAGES", full.names = T)

pblapply(c(imgs, rev(imgs)), image_read) %>% 
  image_join() %>% 
  image_animate(fps = 25) %>% 
  image_write(path = "tuning-c.gif")

unlink("IMAGES", recursive = T)

# Create gif of gaus dist ------------------------------------------------------
library(gganimate)

gaus_anim = data_list$gaus_dist %>% 
  ggplot(aes(x = x, y = g)) +
  geom_line() + 
  labs(
    y = TeX("Gaussian kernel: $\\exp(-(c ||x - y||)^2)$"),
    x = "x - y",
    title = "c = {current_frame}"
  ) + 
  ylim(0, 1) +
  theme_classic() + 
  transition_manual(frames = c) 

anim_save(
  "gaus-dist.gif",
  gaus_anim,
  width = 700, 
  height = 700, 
  res = 150, 
  fps = 25, 
  nframes = data_list$n_frames*2,
  rewind = TRUE
)

# Join the gifs together -------------------------------------------------------
mgif_surf = image_read('tuning-c.gif')
mgif_gaus = image_read('gaus-dist.gif')

pboptions(type = "timer")
pblapply(1:length(mgif_surf), function(i) {
  image_append(c(mgif_surf[i], mgif_gaus[i]))
}) %>% 
  image_join() %>% 
  image_animate(fps = 25, dispose = 'none') %>% 
  image_write(path = 'rbf-tuning.gif')