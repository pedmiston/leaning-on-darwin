# ---- setup
library(readr)
library(dplyr)
library(ggplot2)
library(grid)

df <- read_csv("1-leaning-on-darwin/jacobs_etal_fig2.csv")

df$is_setup <- ifelse(df$participant %in% c(0, 1, 9), 1, 0)
df[(df$is_setup == 1) & (df$group == "experimental"), "is_setup"] <- c(1, 0)

base_plot <- ggplot(df, aes(x = generation, y = estimate, group = participant)) +
  geom_point(aes(shape = group, size = group)) +
  geom_line(aes(lty = group)) +
  coord_cartesian(xlim = c(0.2, 9.8), ylim = c(0, 16.01)) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous("estimate (inches)") +
  scale_shape_manual(values = c(16, 1, 1)) +
  scale_size_manual(values = c(3, 2, 2)) +
  scale_linetype_manual(values = c(1, 2, 1)) +
  theme_minimal(base_size = 24) +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# ---- jacobs-etal-fig2-setup
base_plot %+% filter(df, is_setup == 1)

# ---- jacobs-etal-fig2-preds
trend <- arrow(length = unit(1, "lines"))

preds <- base_plot + 
  annotate("text", x = 7.5, y = 12, label = "depressing!", fontface = "italic") +
  annotate("segment", x = 1.06, y = 12.2, xend = 9, yend = 11.4, arrow = trend) +
  annotate("text", x = 2.3, y = 5.2, label = "hero!", fontface = "italic") +
  annotate("segment", x = 1, y = 12.1, xend = 2, yend = 4.1, arrow = trend)
preds %+% filter(df, is_setup == 1)
  
# ---- jacobs-etal-fig2-data
base_plot +
  geom_point(aes(shape = group, size = group)) +
  geom_line(aes(lty = group))
