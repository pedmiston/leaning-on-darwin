# ---- mountain-climbers
# Plot showing simulations of Diachronic and Synchronic teams.
library(mountainclimbers)
data("differing_skills")

differing_skills %<>%
  recode_fitness_as_pct() %>%
  recode_team() %>%
  extract_position()

# Select analogs for Diachronic, Synchronic, and Isolated
strategy_recode <- data_frame(
  strategy = c("diachronic", "diachronic", "synchronic"),
  team_id = c("a", "e", "e"),
  totems_strategy = c("Isolated", "Diachronic", "Synchronic")
)

proof_of_principle <- differing_skills %>%
  filter(
    (strategy == "diachronic" & team_id %in% c("a", "e")) | 
    (strategy == "synchronic" & team_id == "e")
  ) %>%
  left_join(strategy_recode)

theme_colors <- RColorBrewer::brewer.pal(4, "Set2")
names(theme_colors) <- c("green", "orange", "blue", "pink")
get_theme_color_values <- function(names) theme_colors[names] %>% unname()

scale_y_fitness_pct <- scale_y_continuous("Fitness", labels = scales::percent)
scale_color_strategy <- scale_color_manual(
  "Strategy",
  values = get_theme_color_values(c("blue", "orange", "green"))
)
scale_fill_strategy <- scale_fill_manual(
  "Strategy",
  values = get_theme_color_values(c("blue", "orange", "green"))
)

gg_differing_skills_timeline <- ggplot(proof_of_principle) +
    aes(time, fitness_pct, color = totems_strategy) +
    geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
    scale_x_continuous("Iterations") +
    scale_y_fitness_pct +
    scale_color_strategy +
    guides(color = guide_legend(order = 1)) +
    base_theme +
    theme(legend.position = "none")

max_fitness <- proof_of_principle %>%
  group_by(sim_id, totems_strategy, team_label) %>%
  summarize(fitness_pct = max(fitness_pct))

dodge_width <- 0.9
team_dodge <- position_dodge(width = dodge_width)
sim_dodge <- position_jitterdodge(dodge.width = dodge_width, jitter.width = 0.4)
gg_differing_skills_final_fitness <- ggplot(max_fitness) +
    aes(x = totems_strategy, fitness_pct) +
    scale_x_discrete("Strategy") +
    scale_y_fitness_pct +
    # geom_point(aes(color = strategy), position = sim_dodge) +
    geom_bar(aes(fill = totems_strategy),
             stat = "summary", fun.y = "mean", position = team_dodge,
             alpha = 0.8) +
    scale_fill_strategy +
    guides(fill = "none") +
    base_theme +
    theme(panel.grid.major.x = element_blank())

grid.arrange(gg_differing_skills_timeline,
             gg_differing_skills_final_fitness,
             nrow = 1)

detach("package:mountainclimbers", unload=TRUE)

# ---- mountain-climbers-walks
ggplot(proof_of_principle %>% filter(exp_id == 1)) +
  aes(pos_x, pos_y, group = sim_id, color = totems_strategy) +
  geom_path(alpha = 0.2) +
  annotate("point", x = 0, y = 0, shape = 4) +
  annotate("point", x = -127.3, y = -127.3, shape = 1) +
  coord_equal() +
  facet_wrap("strategy_rev") +
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("", labels = NULL) +
  scale_color_strategy +
  base_theme +
  theme(legend.position = "none") +
  facet_wrap("totems_strategy")
