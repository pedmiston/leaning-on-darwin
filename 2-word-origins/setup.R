# ---- setup
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(tidyr)
library(lme4)
library(AICcmodavg)
library(pander)

# colors
colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("green", "orange", "blue", "pink")
between_color <- colors[["green"]]
within_color <- colors[["blue"]]

# ggplot theme
distractor_labels <- c("Category match\n(true seed)", "Category match", "Specific match")
distractor_colors <- c(between_color, colors[["orange"]], within_color)

scale_y_accuracy <- scale_y_continuous(
  "Guess the seed accuracy",
  labels = percent,
  breaks = seq(0, 1, by = 0.25)
)
scale_x_generation <- scale_x_continuous(
  "Generation",
  breaks = 1:12
)
scale_x_generation_1 <- scale_x_continuous(
  "Generation",
  breaks = 0:11,
  labels = 1:12
)
scale_x_distractors <- scale_x_discrete(
  "",
  labels = distractor_labels
)
scale_fill_distractors <- scale_fill_manual(
  "",
  values = distractor_colors
)
scale_fill_categories <- scale_fill_manual(
  "",
  values = unname(colors)
)
scale_color_distractors <- scale_color_manual(
  "",
  values = distractor_colors,
  labels = distractor_labels
)

chance_line <- geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 1.5)
chance_label <- annotate("text", x = 10, y = 0.26, label = "chance",
                         size = 7, vjust = -0.1, fontface = "italic", alpha = 0.4)

base_theme <- theme_minimal(base_size = 24) +
  theme(axis.ticks = element_blank())

distractor_xlim <- c(-0.2, 7.2)
distractor_coords <- coord_cartesian(
  xlim = distractor_xlim,
  ylim = c(-0.02, 1.02)
)
distractor_diff_coords <- coord_cartesian(
  xlim = distractor_xlim,
  ylim = c(-0.02, 0.8)
)