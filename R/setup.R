# ---- setup
library(tidyverse)
library(stringr)
library(grid)
library(gridExtra)
library(magrittr)
library(lme4)
library(AICcmodavg)

library(crotchet)

base_theme <- theme_minimal(base_size = 18)

colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("blue", "orange", "green", "pink")
get_colors <- function(x) colors[x] %>% unname()

