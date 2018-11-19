
library(magick)
library(tidyverse)
source("helpers.R")

img <- image_read("target/Omayra_Sanchez-square.jpg")


img_cls <- img_kmeans(img, clusters =10)
img_cls
image_write(img_cls$img,"target-cluster-10.jpeg")
img_cls$colors_dist

color_dist <- data_frame(
  color = names(img_cls$colors_dist),
  prop = img_cls$colors_dist

)
write_csv(color_dist,"color_dist-10.csv")

