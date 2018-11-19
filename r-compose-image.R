
library(tidyverse)
library(magick)

srcimgs1 <- list.files("src_img_ori/frases", full.names = TRUE, recursive = TRUE,
                       pattern = "\\.jpg")
srcimgs2 <- list.files("src_img_ori/disasters/", full.names = TRUE, recursive = TRUE,
                       pattern = "\\.png")
srcimgs_caras <- c(srcimgs2,srcimgs2)

available_colors <- read_csv("color_dist-10.csv")

## Copy text images
map(seq_along(srcimgs1), function(i){
  img <- image_read(srcimgs1[i])
  img %>% image_write(path = paste0("srcimgs/txt",i,".jpeg"), format = "jpeg")
})

## Recolor images
map(seq_along(srcimgs_caras), function(i){
  img <- image_read(srcimgs_caras[i])
  img <- img %>% image_scale("200x200!")
  color_cara <- sample(available_colors$color,1,prob = available_colors$prop)
  img <- image_colorize(img, sample(c(30,40,50),1),color_cara)
  img %>% image_write(path = paste0("srcimgs/cara",i,".jpeg"), format = "jpeg")
})




## Resize target img
#target <- "target.png"
target <- "target-cluster-10.jpeg"

#image_read(target) %>% image_write(path = "target.jpeg", format = "jpg")


target_mosaic <- "target-mosaic.jpeg"
image_read(target) %>% image_scale("70x70!") %>%
  image_write(path = target_mosaic, format = "jpeg")

# Make the mosaic

library(RsimMosaic)
composeMosaicFromImageRandom(
  target_mosaic,
  "mosaic.jpg",
  "srcimgs",
  removeTiles=TRUE
)


