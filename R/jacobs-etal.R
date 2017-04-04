source("R/setup.R")

# ---- jacobs-etal-setup
df <- read_csv("jacobs-etal/fig2.csv")

df$is_setup <- ifelse(df$participant %in% c(0, 1, 9), 1, 0)
df[(df$is_setup == 1) & (df$group == "experimental"), "is_setup"] <- c(1, 0)

base_plot <- ggplot(df, aes(x = generation, y = estimate, group = participant)) +
  geom_point(aes(shape = group), size = 3) +
  geom_line(aes(lty = group)) +
  coord_cartesian(xlim = c(0.2, 9.8), ylim = c(0, 16.01)) +
  scale_x_continuous("Generation", breaks = 1:11) +
  scale_y_continuous("Estimate (inches)") +
  scale_shape_manual(values = c(1, 16, 16)) +
  scale_linetype_manual(values = c(1, 2, 1)) +
  theme_minimal(base_size = 24) +
  theme(
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(2, "lines")
  )

# ---- jacobs-etal-fig2-setup
setup_data <- filter(df, is_setup == 1)

label_data <- setup_data %>%
  filter(
    !(group == "control" & generation == 1),
    group != "experimental"
  ) %>%
  mutate(label = str_to_title(group))

base_plot %+% filter(df, is_setup == 1) +
  geom_text(aes(label = label), data = label_data,
            nudge_x = 0.3, hjust = 0, size = 8) +
  theme(legend.position = "none")

# ---- jacobs-etal-fig2-preds
trend <- arrow(length = unit(1, "lines"))

text_size <- 8

preds <- base_plot + 
  annotate("text", x = 7.5, y = 13, label = "depressing!", fontface = "italic", size = text_size) +
  annotate("segment", x = 1.06, y = 12.2, xend = 9, yend = 11.4, arrow = trend) +
  annotate("text", x = 3, y = 5.2, label = "heroic!", fontface = "italic", size = text_size) +
  annotate("segment", x = 1, y = 12.1, xend = 2, yend = 4.1, arrow = trend)
preds %+% filter(df, is_setup == 1)

# ---- jacobs-etal-fig2-data
base_plot +
  geom_point(aes(shape = group)) +
  geom_line(aes(lty = group))
