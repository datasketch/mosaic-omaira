library(magick)
library(tidyverse)
source("helpers.R")

frases <- read_csv("data/Phrases-mosaic Omaira - phrases.csv")
frases <- mop::na_to_empty_chr(frases,empty = " ")

#available_colors <- c("#FA5439", "#DA4589","#5498A0","#45DCC0")
available_colors <- read_csv("color_dist-10.csv")

#bg_color <- "#FA5439"
#create_bg_img(bg_color)
#contrast_color(bg_color)

frases <- bind_rows(frases,frases)

i <<- 1
walk(transpose(frases), function(f){
  #f <- transpose(frases)[[1]]
  message(f[[1]], f[[2]])
  wraps <- c(15,18,30)
  #wraps <- c(20,23,40)
  sizes <- c(14,13,9)
  #sizes <- c(24,16,10)
  h2 <- sample(c(70,50,40),1)
  #h2 <- sample(c(100,130,80,90,60),1)
  text1 <- wrap_sentence(f[[1]], wraps[1])
  text2 <- wrap_sentence(f[[2]],wraps[2])
  text3 <- wrap_sentence(f[[3]],wraps[3])
  #bg_color <- "#FA5439"
  bg_color <- sample(available_colors$color,1,prob = available_colors$prop)
  bg <- create_bg_img(bg_color, width = 200, height = 200)
  text_color <- contrast_color(bg_color)
  img <- bg %>% 
    image_annotate(text1, size = sizes[1], color = text_color, #font = 'Press Start 2P',
                   degrees = 0, location = "+10+10") %>% 
    image_annotate(text2, size = sizes[2], color = text_color, #font = 'Press Start 2P',
                   degrees = 0, location = paste0("+10+",h2)) %>% 
    image_annotate(text3, size = sizes[3], color = text_color, #font = 'Press Start 2P',
                   degrees = 0, location = "+10+180") 
  image_write(img, paste0("src_img_ori/frases/",i,".jpg"))
  i <<- i+1
})




