################################################################################
# merge_example_figures.R — Assemble the worked-example figures
#
# The journal asks for one file per figure with all panels merged. The worked
# examples are drawn as slides (figs/slide_0XX.png); this script tiles them into
# figs/example_absolute.png (2 x 2) and figs/example_relative.png (1 x 2).
################################################################################

library(magick)

tile <- function(slides, ncol, out) {
  imgs <- image_read(file.path("figs", sprintf("slide_%03d.png", slides)))
  rows <- split(seq_along(slides), ceiling(seq_along(slides) / ncol))
  strips <- lapply(rows, function(i) image_append(imgs[i]))
  merged <- image_append(do.call(c, strips), stack = TRUE)
  image_write(merged, file.path("figs", out), format = "png")
  message("Wrote figs/", out)
}

tile(5:8,   ncol = 2, out = "example_absolute.png")
tile(10:11, ncol = 2, out = "example_relative.png")
