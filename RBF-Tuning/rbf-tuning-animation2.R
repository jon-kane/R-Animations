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

# Panel function for wireframe
myPanel = function(x, y, z, xlim, ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled, pts, ...) {
  panel.3dwire(x = x, y = y, z = z, xlim = xlim, ylim = ylim, zlim = zlim,
               xlim.scaled = xlim.scaled, ylim.scaled = ylim.scaled, zlim.scaled = zlim.scaled, ...)
  xx = xlim.scaled[1] + diff(xlim.scaled) * (pts$x - xlim[1]) / diff(xlim)
  yy = ylim.scaled[1] + diff(ylim.scaled) * (pts$y - ylim[1]) / diff(ylim)
  zz = zlim.scaled[1] + diff(zlim.scaled) * (pts$z - zlim[1]) / diff(zlim)
  panel.3dscatter(x = xx, y = yy, z = zz, xlim = xlim, ylim = ylim, zlim = zlim,
                  xlim.scaled = xlim.scaled, ylim.scaled = ylim.scaled, zlim.scaled = zlim.scaled,
                  col = "red", pch = 19, ...)
}

# Create combined frames directly ----------------------------------------------

pboptions(type = "timer")

dir.create("FRAMES", recursive = TRUE, showWarnings = FALSE)

# Resolution Scaler (75% of original)
res_scale = 0.75

orig_w = 1080
orig_h_top = 1350/3
orig_h_bot = (1350/3)*3

new_w = orig_w * res_scale
new_h_top = orig_h_top * res_scale
new_h_bot = orig_h_bot * res_scale

# Define theme to hide the box/lines and REMOVE MARGINS
wire_theme = list(
  box.3d = list(col = "transparent"),    
  axis.line = list(col = "transparent"), 
  par.xlab.text = list(col = "black", cex = 2),
  par.ylab.text = list(col = "black", cex = 2),
  par.zlab.text = list(col = "black", cex = 2, rot = 90),
  # UPDATED: Set all padding to 0 to make the plot fill the space
  layout.heights = list(
    top.padding = 0,
    bottom.padding = 0,
    axis.top = 0,
    axis.bottom = 0,
    main.key.padding = 0
  ),
  layout.widths = list(
    left.padding = 0,
    right.padding = 0,
    axis.left = 0,
    axis.right = 0,
    key.ylab.padding = 0
  )
)

pblapply(1:data_list$n_frames, FUN = function(i) {
  
  # -- 1. Prepare Data --
  tmp_surf = data_list$X_mod %>% filter(index == i)
  current_c = tmp_surf$c[1]
  tmp_gaus = data_list$gaus_dist %>% filter(c == current_c)
  
  # -- 2. Generate Top Image (Gaussian) --
  p_gaus = ggplot(tmp_gaus, aes(x = x, y = g)) +
    geom_line() + 
    labs(
      y = TeX("$\\exp(-(c ||x - y||)^2)$"),
      x = "x - y",
      title = paste0("c = ", round(current_c, 2))
    ) + 
    ylim(0, 1) +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      axis.line.x = element_line(
        arrow = grid::arrow(length = unit(0.3, "cm"), ends = "both")
        ),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  
  fn_top = tempfile(fileext = ".png")
  ggsave(fn_top, p_gaus, width = new_w, height = new_h_top, units = "px", dpi = 150, bg = "transparent")
  
  # -- 3. Generate Bottom Image (Wireframe) --
  fn_bot = tempfile(fileext = ".png")
  
  png(file = fn_bot, width = new_w, height = new_h_bot, bg = "transparent")
  
  ttl = paste0("c = ", current_c)
  
  p_wire = wireframe(
    z_hat ~ x * y, tmp_surf, distance = 0, screen = list(z = 30, x = -70),
    pts = data_list$X_train, panel.3d.wireframe = myPanel,
    xlim = xlim, ylim = ylim, zlim = zlim,
    zlab = TeX("\\hat{f}(x, y)"),
    scales = list(arrows = T, col = "black"), 
    par.settings = wire_theme 
  )
  
  print(p_wire)
  
  # grid.text(ttl, x=unit(0.05, "npc"), y=unit(0.8, "npc"), just = "left", gp = gpar(fontsize = 28))
  
  invisible(dev.off())
  
  # -- 4. Combine and Save --
  img_top = image_read(fn_top)
  img_bot = image_read(fn_bot)
  
  # Geometry format: "width x height + x_offset + y_offset"
  # crop_geo1 = sprintf("%dx%d+0+50", as.integer(new_w), as.integer(new_h_top - 100))
  # img_top = image_crop(img_top, crop_geo1)
  
  # Height is reduced by 200 total; y_offset is 100 to skip top 100px
  crop_geo2 = sprintf("%dx%d+0+275", as.integer(new_w), as.integer(new_h_bot - 400))
  img_bot = image_crop(img_bot, crop_geo2)
  
  img_combined = image_append(c(img_top, img_bot), stack = TRUE)
  
  # img_combined
  
  image_write(img_combined, path = paste0("FRAMES/frame_", sprintf("%04d", i), ".png"))
  
  unlink(c(fn_top, fn_bot))
  
}) %>% invisible()

# Join the images into final GIF -----------------------------------------------
imgs = list.files("FRAMES", full.names = T)

pblapply(c(imgs, rev(imgs)), image_read) %>% 
  image_join() %>% 
  image_animate(fps = 25, dispose = "background") %>% 
  image_write(path = "rbf-tuning.gif")

unlink("FRAMES", recursive = TRUE)