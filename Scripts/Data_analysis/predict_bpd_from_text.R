# Predict BPD by the words used in all answers
library(tidyverse)
library(tidymodels)
library(textrecipes)

groups <- 
  tibble(group = c("gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", "gr8", "gr9"),
         video_valence = c(rep("Positive", 3),
                           rep("Neutral", 3),
                           rep("Negative", 3))) %>% 
  mutate(video_valence = fct_relevel(video_valence, "Neutral"))        

theme_set(theme_light())

sessions_to_exclude <- c(
  # Non-English response
  "qf3LHAoO3suS9hpjgDMR9uf-C-YnLaSlqCrkIVMZbpGTX4bZsVzU6bqevzaiZ80G"
)

df_raw <- read_csv2("Data/data_final_long_scales_181121.csv")


# Process and clean data ------------------------------------------------------------



all_answers <-
  df_raw %>%
  # Exclude invalid sessions (see reasons above)
  filter(!session %in% sessions_to_exclude) %>% 
  select(session, bpd = BPD, matches("gr\\d+_qual")) %>% 
  gather(group_time, answer, -session, -bpd, na.rm = TRUE) %>% 
  separate(group_time, c("group", "time"), sep = "_qual.t", convert = TRUE) 




# Modeling --------------------------------------------------------------------------

# Split data
set.seed(1)
bpd_split <- initial_split(all_answers, strata = group)
bpd_train <- training(bpd_split)
bpd_test <- testing(bpd_split)

# bpd_folds <- bootstraps(bpd_train, strata = "group")
bpd_folds <- vfold_cv(bpd_train, strata = "group")

# Build recipe
# TODO: Add education level as covariate?


bpd_rec <- 
  recipe(bpd ~ answer, data = bpd_train) %>%
  # update_role(session, new_role = "id") %>%
  # update_role(group, time, new_role = "grouping") %>%
  step_tokenize(answer, engine = "spacyr") %>%
  step_lemma(answer) %>%
  # step_stopwords(answer) %>%
  # POS tags: https://spacy.io/api/annotation#pos-tagging
  step_pos_filter(answer, 
                  keep_tags = c("ADJ", "ADP", "ADV", "AUX", "INTJ", "NOUN", "PRON", "PROPN", "SCONJ", "VERB")) %>% 
  step_tfidf(answer) %>%
  step_zv(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_outcomes(), all_predictors())

bpd_prep <- prep(bpd_rec)

juice(bpd_prep)


# Lasso ------------------------------------------------------------------------

lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- 
  workflow() %>%
  add_recipe(bpd_rec) %>%
  add_model(lasso_spec)

lasso_wf

usemodels::use_ranger(formula = bpd ~ answer, 
                      data = bpd_train, 
                      prefix = "rf")

# Tune parameters

lambda_grid <- grid_regular(penalty(), levels = 20)

set.seed(123)
bpd_folds <- rsample::vfold_cv(bpd_train, v = 10)
bpd_folds

doParallel::registerDoParallel()

set.seed(2)
lasso_grid <- tune_grid(
  lasso_wf,
  resamples = bpd_folds,
  grid = lambda_grid,
  metrics = metric_set(rmse, rsq)
)

lasso_grid %>% 
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), 
                alpha = 0.5) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lowest_rmse <- 
  lasso_grid %>% 
  select_best(metric = "rmse")

show_best(lasso_grid, metric = "rmse")

final_lasso <- 
  finalize_workflow(lasso_wf, 
                    parameters = lowest_rmse)


# Random forest ----------------------------------------------------------------

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

rf_wf <- 
  workflow() %>% 
  add_recipe(bpd_rec) %>% 
  add_model(rf_spec) 

set.seed(26938)

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

rf_res <- tune_grid(rf_wf, resamples = bpd_folds, grid = rf_grid)


rf_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(.config, mean, min_n, mtry) %>% print(n = 100)
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, 
         color = parameter)) +
  geom_point(show.legend = FALSE) +
  geom_smooth() +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

lowest_rmse_rf <- 
  tune_res %>% 
  select_best(metric = "rmse")

show_best(lasso_grid, metric = "rmse")

final_rf <- 
  finalize_workflow(rf_wf, 
                    parameters = lowest_rmse_rf)

# Evaluate ---------------------------------------------------------------------



library(vip)

final_lasso %>% 
  fit(bpd_train) %>% 
  pull_workflow_fit() %>% 
  vi() %>%
  mutate(Importance = abs(Importance),
         Variable = str_remove(Variable, "tfidf_answer_") %>% 
                    fct_reorder(Importance)) %>%
  filter(Importance > .08) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "Importance of wordstems in predicting borderline trait",
       subtitle = "Only wordstems with larger than .08 importance are shown. Stopwords were removed.",
       y = NULL)

lasso_rs <- 
  final_lasso %>% 
  fit_resamples(bpd_folds,
                control = control_resamples(save_pred = TRUE)) 

# Performance on training set
collect_metrics(lasso_rs)

# Performance on test set
last_fit(final_lasso, bpd_split) %>%
  collect_metrics()

predict(final_lasso)
