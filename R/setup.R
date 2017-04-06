# ---- setup
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(scales)

library(grid)
library(gridExtra)
library(png)

library(lme4)
library(AICcmodavg)

library(crotchet)

base_theme <- theme_minimal(base_size = 18)

colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("blue", "orange", "green", "pink")
get_colors <- function(x) colors[x] %>% unname()

img <- function(name, draw = TRUE, ...) {
  img_grob <- paste0("img/", name, ".png") %>%
    png::readPNG() %>%
    grid::rasterGrob(...)
  if (draw) {
    grid::grid.newpage()
    grid::grid.draw(img_grob)
  }
  img_grob
}