library(magrittr)
library(lattice)
library(pbapply)
library(parallel)

data_list = readRDS("data-list.RDS")

# Create a temporay directory
dir.create("IMAGES", recursive = T, showWarnings = F)

pboptions(type = "timer")

cl = makeForkCluster(detectCores())

pblapply(cl = cl, X = 1:length(data_list), FUN = function(i) {
  df = data_list[[i]]
  
  fn = paste0("IMAGES/ts", sprintf("%04d", i), ".png")
  
  png(file = fn, width = 300, height = 300)
  
  trellis.par.set("box.3d", list(col = "transparent"))
  trellis.par.set("axis.line", list(col = "transparent"))
  trellis.par.set("par.xlab.text", list(col = "transparent"))
  trellis.par.set("par.ylab.text", list(col = "transparent"))
  trellis.par.set("par.zlab.text", list(col = "transparent"))
  p = wireframe(y~x1*x2, df)
  
  print(p)
  
  invisible(dev.off())
}) %>% invisible()

stopCluster(cl)

# Creates a gif using Imagemagick
# system_args = paste0(
#   "convert -delay 3 IMAGES/*.png ",
#   "-colorspace rgb -colorspace srgb -quantize lab -fuzz 3% +dither ",
#   "-layers optimize ",
#   "drop-wave.gif"
# )

# Creates an mp4 using ffmpeg
system_args = paste0(
  "ffmpeg -r 30 -f image2 -s 300x300 -i IMAGES/ts%04d.png ",
  "-vcodec libx264 -crf 25  -pix_fmt yuv420p drop-wave.mp4"
)

system(system_args)

unlink("IMAGES", recursive = T)
