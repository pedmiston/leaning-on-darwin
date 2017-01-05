# ---- models
responses_mod <- glmer(is_correct ~ generation_1 * (same_v_between + same_v_within) + 
                         (generation_1|chain_name/seed_id),
                       family = "binomial", data = responses)

x_preds <- expand.grid(
  generation_1 = unique(responses$generation_1),
  survey_type = c("between", "same", "within"),
  stringsAsFactors = FALSE
) %>% left_join(survey_map) %>%
  mutate(
    generation = generation_1 + 1,
    generation_label = paste("Generation", generation)
  )

transition_preds <- predictSE(responses_mod, x_preds, se = TRUE) %>%
  cbind(x_preds, .) %>%
  rename(is_correct = fit, se = se.fit)

matches_mod <- glmer(is_correct ~ question_c + (1|text_category),
                     family = "binomial", data = matches)

x_preds <- data.frame(question_c = c(-0.5, 0.5)) 
matches_preds <- predictSE(matches_mod, x_preds, se = TRUE) %>%
  cbind(x_preds, .) %>%
  rename(is_correct = fit, se = se.fit) %>%
  left_join(question_type_map)