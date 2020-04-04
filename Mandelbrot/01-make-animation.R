fps = 30; img_res = 800

files = list.files()

if (!is.na(match("mandelbrot.mp4", files))) {
  system("rm mandelbrot.mp4")
}

system_args = paste0(
  "ffmpeg -r ", fps, " -f image2 -s ", img_res, "x", img_res, 
  " -i IMAGES/ts%04d.png",
  " -vcodec libx264 -crf 25 -pix_fmt yuv420p mandelbrot.mp4"
)

system(system_args)

unlink("IMAGES", recursive = T)
