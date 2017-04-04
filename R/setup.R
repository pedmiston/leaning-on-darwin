# ---- setup
library(tidyverse)
library(grid)
library(magrittr)
library(lme4)
library(AICcmodavg)

library(crotchet)

base_theme <- theme_minimal()

colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("blue", "orange", "green", "pink")
get_colors <- function(x) colors[x] %>% unname()
