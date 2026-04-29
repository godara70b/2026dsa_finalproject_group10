class_packages <- c(
  # core modeling
  "tidymodels",
  "finetune",
  
  # data wrangling
  "tidyverse",
  "janitor",
  "stringr",
  "lubridate",
  
  # models
  "ranger",
  "glmnet",
  "xgboost",
  
  # utilities
  "vip",
  "nasapower",
  "here",
  
  # parallel
  "doParallel",
  
  "lme4",
  
  "future",
  
  "embed",
  
  "recipes"
)

# shared_lib_path <- "/work/crss8030/instructor_data/shared_R_libs"

# class_packages <- c("tidymodels", "tidyverse", "vip", "ranger", "finetune", "parsnip", "reticulate", "xgboost", "doParallel", "lme4", "here")
# 
# install.packages(class_packages, lib = shared_lib_path)

# .libPaths(c(shared_lib_path, .libPaths()))


shared_lib_path <- "/home/tao92719/shared_R_libs"
.libPaths(shared_lib_path)
# confirm
.libPaths()

# # install missing packages
# installed <- installed.packages(lib.loc = shared_lib_path)[, "Package"]
# to_install <- class_packages[!class_packages %in% installed]
# if (length(to_install)) install.packages(to_install, lib = shared_lib_path, dependencies = T, repos = "https://cloud.r-project.org")

# install once if needed
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# install.packages("xgboost", lib = shared_lib_path)


# unlink("/home/tao92719/shared_R_libs/00LOCK-xgboost", recursive = TRUE, force = TRUE)
# install.packages("xgboost", lib = "/home/tao92719/shared_R_libs", repos = "https://cloud.r-project.org")

library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(here)
library(janitor)
library(lubridate)
library(nasapower)
library(purrr)
library(vip)
library(future)  # for parallel
#install.packages("embed", lib = shared_lib_path)
library(embed)
library(recipes)

theme_set(theme_bw())


# helper functions
extract_site_base <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_extract("[a-z]{2,3}h?\\d+[a-z]?") %>%
    stringr::str_to_upper()
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# read and clean raw data
train_meta  <- read_csv(here("data/training", "training_meta.csv"),  show_col_types = FALSE) %>%
  clean_names()
train_soil  <- read_csv(here("data/training", "training_soil.csv"),  show_col_types = FALSE) %>%
  clean_names()
train_trait <- read_csv(here("data/training", "training_trait.csv"), show_col_types = FALSE) %>%
  clean_names()

test_meta   <- read_csv(here("data/testing", "testing_meta.csv"), show_col_types = FALSE) %>%
  clean_names()
test_soil   <- read_csv(here("data/testing", "testing_soil.csv"), show_col_types = FALSE) %>%
  clean_names()
test_sub    <- read_csv(here("data/testing", "testing_submission.csv"), show_col_types = FALSE) %>%
  clean_names()

# clean test data - submission + meta + soil
test_sub_clean <- test_sub %>%
  mutate(
    site_clean = extract_site_base(site),
    year       = 2024
  )

test_meta_clean <- test_meta %>%
  mutate(
    site_clean    = extract_site_base(site),
    previous_crop = str_to_lower(previous_crop)
  ) %>%
  distinct(site_clean, year, .keep_all = TRUE)

test_soil_clean <- test_soil %>%
  mutate(site_clean = extract_site_base(site)) %>%
  group_by(site_clean, year) %>%
  summarise(
    soilpH    = safe_mean(soilp_h),
    om_pct    = safe_mean(om_pct),
    soilk_ppm = safe_mean(soilk_ppm),
    soilp_ppm = safe_mean(soilp_ppm),
    .groups   = "drop"
  )

# Load latest weather file and clean
files <- list.files(
  here("data"),
  pattern = "weather_season_summary_.*\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No weather_season_summary_*.csv file found in data/.")
}

latest_file <- files[which.max(file.info(files)$mtime)]
weather_all <- read_csv(latest_file, show_col_types = FALSE)

weather2 <- weather_all %>%
  mutate(site_clean = str_trim(site_clean)) %>%
  distinct(site_clean, year, .keep_all = TRUE)


# combine train trait + weather + soil data
train_trait2 <- train_trait %>%
  mutate(site_clean = str_extract(site, "^[A-Z0-9]+"))

train_joined <- train_trait2 %>%
  left_join(weather2, by = c("site_clean", "year"))

train_final <- train_joined %>%
  filter(!is.na(gdd_total))

train_soil2 <- train_soil %>%
  mutate(
    site_clean = str_remove(site, "_\\d{4}$"),
    year       = as.integer(str_extract(site, "\\d{4}$"))
  ) %>%
  group_by(site_clean, year) %>%
  summarise(
    soilpH    = safe_mean(soilp_h),
    om_pct    = safe_mean(om_pct),
    soilk_ppm = safe_mean(soilk_ppm),
    soilp_ppm = safe_mean(soilp_ppm),
    .groups   = "drop"
  ) %>%
  # select(-site) %>%
  distinct(site_clean, year, .keep_all = TRUE)

train_df <- train_final %>%
  left_join(train_soil2, by = c("site_clean", "year"))

glimpse(train_df)
cat("Training rows:", nrow(train_df), " | columns:", ncol(train_df), "\n")


# combine test submission + meta + soil + weather data
test_df <- test_sub_clean %>%
  left_join(test_meta_clean, by = c("site_clean", "year")) %>%
  left_join(test_soil_clean, by = c("site_clean", "year")) %>%
  left_join(weather2,        by = c("site_clean", "year")) %>%
  mutate(
    previous_crop = coalesce(previous_crop.x, previous_crop.y),
    longitude     = coalesce(longitude.x, longitude.y),
    latitude      = coalesce(latitude.x, latitude.y),
    previous_crop = replace_na(previous_crop, "unknown"),
    across(
      c(soilpH, om_pct, soilk_ppm, soilp_ppm),
      ~ ifelse(is.na(.), safe_mean(.), .)
    )
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"), -site.x, -site.y)

cat("Test rows:", nrow(test_df), "\n")


# convert to factors
train_df <- train_df %>%
  mutate(
    site          = factor(site_clean),
    hybrid        = factor(hybrid),
    previous_crop = factor(previous_crop),
    year          = factor(year),
    site_year     = paste(site_clean, year, sep = "_")
  )

test_df <- test_df %>%
  mutate(
    site          = factor(site_clean),
    hybrid        = factor(hybrid),
    previous_crop = factor(previous_crop),
    year          = factor(year),
    site_year     = paste(site_clean, year, sep = "_")
  )


# yield distribution
density_plot <- ggplot(train_df, aes(x = yield_mg_ha)) +
  geom_density(fill = "steelblue", color = "white", alpha = 0.6) +
  labs(
    title = "Distribution of Training Yields",
    x = "Yield (Mg/ha)", 
    y = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# print status
cat("Saving...... density plot of train data\n")

# save plot
ggsave(plot = density_plot, 
       path = here("output", "png"),
       filename = "density_plot_train.png",
       height = 6,
       width = 9,
       dpi = 600)


# Recipe for BOTH XGBoost and GLMNET
predictors <- c("year", "site_clean", "previous_crop", "longitude", "latitude", 
                "soilpH", "om_pct", "soilk_ppm", "soilp_ppm", "tmean_season", 
                "tmax_mean", "tmin_mean", "heat_days", "gdd_total", 
                "precip_total", "srad_mean", "hybrid")

# corn_rec <- recipe(yield_mg_ha ~ ., data = train_df) %>%
corn_rec <- recipe(as.formula(paste("yield_mg_ha ~", paste(predictors, collapse = " + "))), data = train_df) %>%
  step_rm(soilpH, om_pct, soilk_ppm, soilp_ppm) %>% # Drop columns that are 100% NA
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_lencode_glm(all_nominal_predictors(), outcome = vars(yield_mg_ha)) %>%
  step_impute_median(all_numeric_predictors()) %>% # Impute remaining NA in other numeric columns
  step_dummy(all_nominal_predictors(), -hybrid)

# corn_rec <- recipe(as.formula(paste("yield_mg_ha ~", paste(predictors, collapse = " + "))), data = train_df) %>%
#   step_novel(all_nominal_predictors()) %>%
#   step_unknown(all_nominal_predictors()) %>%
#   step_lencode_glm(all_nominal_predictors(), outcome = vars(yield_mg_ha)) %>% # Add the outcome here
#   step_dummy(all_nominal_predictors())

# corn_rec <- recipe(
#   as.formula(paste("yield_mg_ha ~", paste(predictors, collapse = " + "))),
#   data = train_df
# ) %>%
#   step_unknown(previous_crop, new_level = "unknown") %>%
#   step_indicate_na(soilpH, om_pct, soilk_ppm, soilp_ppm) %>%
#   step_impute_median(all_numeric_predictors()) %>%
#   step_lencode_glm(hybrid, outcome = vars(yield_mg_ha)) %>%
#   step_novel(all_nominal_predictors()) %>%
#   step_dummy(all_nominal_predictors(), -hybrid)

# corn_prep <- prep(corn_rec)
# p <- juice(corn_prep) %>%
#   select(-yield_mg_ha) %>%
#   ncol()
# 
# cat("Number of post-recipe predictors:", p, "\n")


# First model - XGBoost
# XGBoost model spec and workflow
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune(),
  min_n          = tune(),
  mtry           = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(corn_rec)


# resampling
set.seed(123)
resampling_foldcv <- vfold_cv(train_df, v = 3)


# tuning grid
xgb_grid <- grid_space_filling(
  tree_depth(),
  learn_rate(range = c(-3, -0.5)),
  trees(range = c(200L, 500L)),
  loss_reduction(),
  min_n(),
  mtry(range = c(1L, 15L)),
  size = 10
)

xgb_grid

# create xgb_grid plot
xgb_grid_plot <- ggplot(xgb_grid, aes(x = tree_depth, y = min_n)) +
  geom_point(
    aes(color = round(learn_rate, 3), size = trees),
    alpha = 0.6, show.legend = TRUE # Set to TRUE to see what the values mean
  ) +
  labs(
    title = "XGBoost Hyperparameter Tuning Grid",
    subtitle = "Tree depth vs. min_n (colored by learning rate, sized by number of trees)",
    x = "Tree Depth",
    y = "Min N"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# show the plot
xgb_grid_plot

# save the plot
cat("Saving...... XGBoost tuning grid visualization\n")

ggsave(plot = xgb_grid_plot, 
       path = here("output", "png"),
       filename = "xgb_tuning_grid.png",
       height = 6,
       width = 9,
       dpi = 600)


library(doParallel)
# detect cores and register parallel backend
n_cores <- parallel::detectCores()

cl <- makeCluster(max(1, n_cores - 1))
registerDoParallel(cl)

set.seed(123)
results <- tune_grid(
  xgb_wf,
  resamples = resampling_foldcv,
  grid      = xgb_grid,
  metrics   = metric_set(rmse, rsq),
  control   = control_grid(
    verbose       = TRUE,
    save_pred     = TRUE,
    parallel_over = "resamples"  # parallel over folds
  )
)

# stop cluster when done
parallel::stopCluster(cl)
library(foreach)
registerDoSEQ() # return to sequential

# collect metrics
all_metrics     <- collect_metrics(results)
summary_metrics <- collect_metrics(results, summarize = TRUE)
fold_metrics    <- collect_metrics(results, summarize = FALSE)

all_metrics
summary_metrics

# best configuration by RMSE and R2
best_rmse <- select_best(results, metric = "rmse")
best_r2   <- select_best(results, metric = "rsq")

best_xgb <- best_rmse
best_xgb

# final model fit on full training data
final_xgb_wf  <- finalize_workflow(xgb_wf, best_xgb)
final_xgb_fit <- fit(final_xgb_wf, data = train_df)

# training diagnostics
train_pred <- augment(final_xgb_fit, new_data = train_df)

train_metrics <- bind_rows(
  train_pred %>% rmse(truth = yield_mg_ha, estimate = .pred),
  train_pred %>% rsq(truth = yield_mg_ha, estimate = .pred)
)

train_metrics

# # tune XGBoost - sequential
# library(future)
# plan(sequential)
# gc()
# 
# set.seed(123)
# results <- tune_grid(
#   xgb_wf,
#   resamples = resampling_foldcv,
#   grid      = xgb_grid,
#   metrics   = metric_set(rmse, rsq),
#   control   = control_grid(
#     verbose   = TRUE,
#     save_pred = TRUE
#   )
# )
# 
# all_metrics    <- collect_metrics(results)
# summary_metrics <- collect_metrics(results, summarize = TRUE)
# fold_metrics    <- collect_metrics(results, summarize = FALSE)
# 
# all_metrics
# summary_metrics
# 
# # best configuration by RMSE
# best_rmse <- select_best(results, metric = "rmse")
# best_r2   <- select_best(results, metric = "rsq")
# 
# best_xgb <- best_rmse
# best_xgb
# 
# 
# # final model fit
# final_xgb_wf  <- finalize_workflow(xgb_wf, best_xgb)
# final_xgb_fit <- fit(final_xgb_wf, data = train_df)
# 
# # training diagnostics
# train_pred <- augment(final_xgb_fit, new_data = train_df)
# 
# train_metrics <- bind_rows(
#   train_pred %>% rmse(truth = yield_mg_ha, estimate = .pred),
#   train_pred %>% rsq(truth = yield_mg_ha, estimate = .pred)
# )
# 
# train_metrics

publication_ready <- train_pred %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point(aes(fill = yield_mg_ha), shape = 21, alpha = 0.7, show.legend = FALSE) +
  scale_fill_viridis_c(option = "H") +
  geom_abline(color = "red", linetype = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "XGBoost Training Diagnostics",
    x = "Observed Yield (Mg/ha)",
    y = "Predicted Yield (Mg/ha)"
  ) +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = paste0(
      "R-sq: ", round(train_metrics$.estimate[train_metrics$.metric == "rsq"], 3), "\\n",
      "RMSE: ", round(train_metrics$.estimate[train_metrics$.metric == "rmse"], 3)
    ),
    hjust = 1.1, vjust = -0.5
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

publication_ready

# save plot
cat("Saving...... XGBoost prediction performance plot\n")

ggsave(plot = publication_ready, 
       path = here("output", "png"),
       filename = "xgb_prediction_performance.png",  # Fixed filename
       height = 6,
       width = 9,
       dpi = 600)


# variable importance
vip_plot <- final_xgb_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20) +
  labs(
    title = "XGBoost Variable Importance",
    subtitle = "Top 20 predictors by contribution to yield model"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# show plot
print(vip_plot)

# save VIP plot
cat("Saving...... XGBoost variable importance plot\n")

ggsave(plot = vip_plot, 
       path = here("output", "png"),
       filename = "xgb_variable_importance.png",
       height = 8,
       width = 10,
       dpi = 600)


# predict 2024 test and complete test data submission
# test_df <- test_df %>%
#   mutate(year = factor(year, levels = levels(train_df$year)))

xgb_preds <- augment(final_xgb_fit, new_data = test_df)

submission_out <- test_sub %>%
  mutate(row_id = row_number()) %>%
  left_join(
    xgb_preds %>% select(.pred) %>% mutate(row_id = row_number()),
    by = "row_id"
  ) %>%
  mutate(
    yield_mg_ha = coalesce(yield_mg_ha, .pred)
  ) %>%
  select(-row_id, -.pred)

dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)

write_csv(
  submission_out,
  here("output", "testing_submission_filled_xgb.csv")
)


# second model - GLMNET
# glmnet-spec
glmnet_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

glmnet_wf <- workflow() %>%
  add_model(glmnet_spec) %>%
  add_recipe(corn_rec)

glmnet_spec


# glmnet-grid
set.seed(123)

glmnet_grid <- grid_regular(
  penalty(range = c(-4, 1)),
  mixture(range = c(0, 1)),
  levels = c(20, 6)
)
glmnet_grid


# glmnet-grid-plot
# GLMNET tuning grid plot
glmnet_grid_plot <- ggplot(glmnet_grid, aes(x = mixture, y = penalty)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  scale_y_log10() +  # Critical for GLMNET tuning grids
  labs(
    title = "GLMNET Tuning Grid",
    subtitle = "Log-scale penalty vs. L1/L2 mixture",
    x = "Mixture (0 = Ridge, 1 = Lasso)",
    y = "Penalty (log10 scale)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) # Cleaner look

# show plot
glmnet_grid_plot

# save plot
cat("Saving...... GLMNET tuning grid visualization\n")

ggsave(plot = glmnet_grid_plot, 
       path = here("output", "png"),
       filename = "glmnet_tuning_grid.png",
       height = 6,
       width = 8,
       dpi = 600)

# GLMNET tuning with parallel over resamples
# Parallel backend
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_ON_NODE"))
if (is.na(n_cores)) n_cores <- max(1, parallel::detectCores() - 1)

cl <- parallel::makePSOCKcluster(n_cores)
doParallel::registerDoParallel(cl)

cat("Registered", n_cores, "cores for glmnet\n")

# tune glmnet (parallel over CV folds)
set.seed(123)
glmnet_results <- tune_grid(
  glmnet_wf,
  resamples = resampling_foldcv,   # same CV object as XGBoost
  grid      = glmnet_grid,
  metrics   = metric_set(rmse, rsq),
  control   = control_grid(
    verbose       = TRUE,
    save_pred     = TRUE,
    parallel_over = "resamples"
  )
)

# stop cluster when done
parallel::stopCluster(cl)
library(foreach)
registerDoSEQ()


# GLMNET metrics and best parameters
glmnet_all_metrics     <- collect_metrics(glmnet_results)
glmnet_summary_metrics <- collect_metrics(glmnet_results, summarize = TRUE)
glmnet_fold_metrics    <- collect_metrics(glmnet_results, summarize = FALSE)

glmnet_all_metrics
glmnet_summary_metrics


# best parameter sets by RMSE and R²
glmnet_best_rmse <- glmnet_results %>%
  select_best(metric = "rmse") %>%
  mutate(source = "best_rmse")

glmnet_best_r2 <- glmnet_results %>%
  select_best(metric = "rsq") %>%
  mutate(source = "best_r2")

bind_rows(glmnet_best_rmse, glmnet_best_r2) %>%
  select(source, everything())


# choose RMSE as primary selection criterion
best_glmnet <- glmnet_best_rmse %>%
  select(-source)

best_glmnet


# final GLMNET fit on all training data
final_glmnet_wf  <- finalize_workflow(glmnet_wf, best_glmnet)
final_glmnet_fit <- fit(final_glmnet_wf, data = train_df)

glmnet_train_pred <- augment(final_glmnet_fit, new_data = train_df)

glmnet_train_metrics <- bind_rows(
  glmnet_train_pred %>% rmse(truth = yield_mg_ha, estimate = .pred),
  glmnet_train_pred %>% rsq(truth = yield_mg_ha, estimate = .pred)
)

glmnet_train_metrics


# GLMNET predicted vs observed (training diagnostics)
glmnet_plot <- glmnet_train_pred %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point(aes(fill = yield_mg_ha), shape = 21, alpha = 0.7, show.legend = FALSE) +
  scale_fill_viridis_c(option = "H") +
  geom_abline(color = "red", linetype = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GLMNET Training Diagnostics",
    x = "Observed Yield (Mg/ha)",
    y = "Predicted Yield (Mg/ha)"
  ) +
  annotate(
    "label",
    x = Inf, y = -Inf,
    label = paste0(
      "R-sq: ", round(glmnet_train_metrics$.estimate[glmnet_train_metrics$.metric == "rsq"], 3), "\n",
      "RMSE: ", round(glmnet_train_metrics$.estimate[glmnet_train_metrics$.metric == "rmse"], 3)
    ),
    hjust = 1.1, vjust = -0.5
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

# show plot
glmnet_plot

# save plot
cat("Saving...... GLMNET prediction performance plot\n")

ggsave(plot = glmnet_plot, 
       path = here("output", "png"),
       filename = "glmnet_prediction_performance.png",
       height = 6,
       width = 9,
       dpi = 600)


# GLMNET coefficients
glmnet_coefs <- final_glmnet_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate)))

glmnet_coefs %>%
  slice_head(n = 20)

# coefficient plot
glmnet_coef_plot <- glmnet_coefs %>%
  slice_max(order_by = abs(estimate), n = 20) %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = term, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
  labs(
    title = "Top 20 GLMNET Coefficients",
    subtitle = "Positive effects (blue) vs Negative effects (red)",
    x = "Coefficient Estimate",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# show plot
glmnet_coef_plot

# save plot
cat("Saving...... GLMNET coefficient importance plot\n")

ggsave(plot = glmnet_coef_plot, 
       path = here("output", "png"),
       filename = "glmnet_coefficients.png",
       height = 8,
       width = 10,
       dpi = 600)


# GLMNET predictions for 2024 and submission
# Make year factor levels match
# test_df <- test_df %>%
#   mutate(year = factor(year, levels = levels(train_df$year)))

glmnet_preds <- predict(final_glmnet_fit, new_data = test_df) %>%
  bind_cols(test_df %>% select(year, site_clean, hybrid)) %>%
  rename(pred_yield_mg_ha = .pred)

head(glmnet_preds)

glmnet_submission_out <- test_sub %>%
  mutate(yield_mg_ha = glmnet_preds$pred_yield_mg_ha)

write_csv(
  glmnet_submission_out,
  here("output", "testing_submission_filled_glmnet_2.csv")
)

write_csv(glmnet_all_metrics,   here("output", "glmnet_cv_metrics_summary.csv"))
write_csv(glmnet_train_metrics, here("output", "glmnet_train_metrics.csv"))
write_csv(best_glmnet,          here("output", "glmnet_best_params.csv"))
write_csv(glmnet_coefs,         here("output", "glmnet_coefficients.csv"))


# compare XGBoost vs GLMNET (CV-based)
xgb_summary <- all_metrics %>%
  filter(.metric %in% c("rmse", "rsq")) %>%
  group_by(.metric) %>%
  slice_min(mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(model = "XGBoost")
write_csv(xgb_summary,   here("output", "xgb_summary_metrics_summary.csv"))

glmnet_summary <- glmnet_all_metrics %>%
  filter(.metric %in% c("rmse", "rsq")) %>%
  group_by(.metric) %>%
  slice_min(mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(model = "GLMNET")
write_csv(glmnet_summary,   here("output", "glmnet_summary_metrics_summary.csv"))

# bind_rows(xgb_summary, glmnet_summary) %>%
#   select(model, .metric, mean, std_err, n, dplyr::everything())

# combined metrics dataframe
all_models_summary <- bind_rows(xgb_summary, glmnet_summary) %>%
  select(model, .metric, mean, std_err, n, dplyr::everything())

# save combined comparison table
write_csv(all_models_summary, here("output", "model_comparison_summary.csv"))

# show final summary
all_models_summary
