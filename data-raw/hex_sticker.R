

library(hexSticker)
sticker(
  subplot      = "data-raw/TAG NIHR LONG.png",
  s_x          = 1,        # horizontal position of image
  s_y          = 0.8,      # vertical position of image
  s_width      = 0.85,      # image width
  s_height     = 0.85,      # image height
  package      = "msdes",    # text label
  p_size       = 42,       # label font size
  p_color      = "#003B6F",
  p_y          = 1.35,     # vertical position of label
  h_fill       = "white", # hex background
  h_color      = "#003B6F",   # hex border
  filename     = "man/figures/logo.png"
)

