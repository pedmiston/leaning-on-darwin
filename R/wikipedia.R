source("R/setup.R")

# ---- wikipedia-quality
library(wikischolarlib)
data("random1000")

tidy_lmer_coefs <- function(lmer_mod) {
  rand_effects <- coef(lmer_mod)[[1]] %>% as.data.frame
  rand_effects$title <- rownames(rand_effects)
  rownames(rand_effects) <- NULL
  rand_effects
}

library(magrittr)
library(dplyr)

random1000 %<>%
  recode_age %>%
  recode_generations

library(ggplot2)
library(lme4)
library(broom)

color_scheme <- RColorBrewer::brewer.pal(3, "Set2")

gg_base <- ggplot(mapping = aes(x = age, y = quality)) +
  scale_x_continuous("Age of article (years)", breaks = seq(0, 10, 2)) +
  scale_y_continuous("Quality estimate (ML)") +
  scale_color_manual(values = color_scheme) +
  base_theme +
  theme(
    legend.position = "none",
    axis.ticks = element_blank()
  )

# Article quality by age

linear <- lmer(quality ~ age + (age|title), data = random1000)
quad <- lmer(quality ~ age + age_sqr + (age + age_sqr|title), data = random1000)

quad_preds <- unique(random1000[, c("age", "age_sqr")]) %>%
  na.omit() %>%
  cbind(., AICcmodavg::predictSE(quad, newdata = ., se = TRUE)) %>%
  rename(quality = fit, se = se.fit)

quality_by_age_plot <- (gg_base %+% random1000) +
  geom_line(aes(group = title), color = color_scheme[3], alpha = 0.2) +
  geom_line(data = quad_preds, color = color_scheme[2], size = 1.5) +
  geom_point(stat = "summary", fun.y = "mean", size = 3, shape = 1) +
  ggtitle("Quality by article age")

# Article quality by generations

oldest <- random1000 %>%
  group_by(title) %>%
  filter(year == max(year)) %>%
  mutate(
    generations_sum_log10 = log10(generations_sum),
    generations_sum_sqr_log10 = log10(generations_sum_sqr)
  ) %>%
  ungroup()

generations <- lm(quality ~ generations_sum_log10, data = oldest)

generations_preds <- oldest %>%
  select(generations_sum, generations_sum_sqr,
         generations_sum_log10, generations_sum_sqr_log10) %>%
  unique() %>%
  filter(generations_sum > 2,
         generations_sum < 1000) %>%
  cbind(., predict(generations, newdata = ., se = TRUE)) %>%
  rename(quality = fit, se = se.fit)

set.seed(531)  # fix jittered points

quality_by_generations_plot <- (gg_base %+% oldest) +
  aes(x = generations_sum, y = quality) +
  geom_point(position = position_jitter(width = 0, height = 0),
             alpha = 0.4, color = color_scheme[1]) +
  geom_line(data = generations_preds,
            color = color_scheme[2], size = 1.2) +
  scale_x_log10("Generations of edits", breaks = c(1, 10, 100, 1000)) +
  ggtitle("Quality by generations of edits")

grid.arrange(
  quality_by_age_plot,
  quality_by_generations_plot,
  nrow = 1
)
