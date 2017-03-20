devtools::install_github('GuangchuangYu/hexSticker')

library(hexSticker)

sticker("avatar-white.png", package = "Appsilon Data Science",
        p_size = 13, s_x = 1, s_y = 0.75, s_width = .8, s_height = .45,
        filename="sticker.png", h_fill = "#007bcf", h_color = "#9b9b9b",
        p_family = "lato")
