#' Run Random Forest
#'
#' @param data_df 
#' @param response_variable 
#' @param skip_vars 
#'
#' @return
#' @export
#'
#' @examples
model_iNat_RF <- function(data_df, response_variable, 
                          skip_vars, log_vars) {
  
  #######################################################
  ###### 1) DEFINE RECIPE AND MODEL
  
  set.seed(123) # useful to do the same split for spatial cross-validation
  data_split <- initial_split(data_df, prop = 3/4, 
                              strata = !!sym(response_variable))
  data_train <- training(data_split)
  data_test <- testing(data_split) # for estimating performance later
  
  # recipe
  rf_recipe <- recipe(formula = as.formula(paste(response_variable, 
                                                 '~ .')),
                      data = data_train) %>%
    update_role(country_code, new_role = 'ID') %>%
    update_role(all_of(skip_vars), new_role = 'skip') %>% 
    step_log(all_of(log_vars), base = 10) %>% 
    step_normalize(all_predictors())
  
  # define model 
  rf_model <- rand_forest(trees =  tune(), # to find the optimal value
                          min_n = tune(), # to find the optimal value
                          mtry = tune() # to find the optimal value  
  ) %>%
    set_mode('regression') %>%
    set_engine('ranger')
  
  # workflow
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model) 
  
  ### create cross validation folds for tunning
  set.seed(234)
  random_folds <- vfold_cv(data_train, v = 5, 
                           strata = !!sym(response_variable), 
                           repeats = 3)
  
  # get parameters from the training set 
  prep_recipe <- prep(rf_recipe)
  
  # number of predictors in the training set 
  n_pred <- prep_recipe %>% summary() %>% filter(role == 'predictor') %>% nrow()

  # grid for tunning 
  rf_grid <- expand.grid(
    trees = c(500,1000),
    mtry  = seq(2, floor(sqrt(n_pred))*2), # randomly selected predictors
    min_n = c(1, 2, 5, 10, 20) # minimum data points required in a node for further splitting
  )
  
  # register the future backend with foreach (required by tidymodels)
  registerDoFuture()
  set.seed(12345, kind = "L'Ecuyer-CMRG")
  
  plan(multisession, workers = 8)
  
  # parallelise over resamples
  ctrl <- control_grid(parallel_over = 'resamples', 
                       verbose = FALSE,
                       save_workflow = TRUE)
  
  start <- Sys.time()
  
  rf_tune <- rf_workflow %>% 
    tune_grid(resamples = random_folds, grid = rf_grid, control = ctrl)
  
  end <- Sys.time()
  print(end-start)
  
  plan(sequential)
  
  # visualize tuning result
  plot_tuning_params <- autoplot(rf_tune) + 
    theme_bw() +
    ggtitle(str_glue('Tuning results {response_variable}' ))
  
  # select the best parameters
  best_params <- rf_tune %>% select_best(metric = 'rmse')
  
  # show the best hyperparameters
  # show_best(rf_tune, metric = 'rsq')
  print(paste0('best tunning parameters: mtry=', best_params$mtry, ', min_n=', best_params$min_n))
  print(show_best(rf_tune, metric = 'rsq')[1,])
  print(show_best(rf_tune, metric = 'rmse')[1,])
  
  # finalise workflow with the best parameters
  final_rf_workflow <- rf_workflow %>% 
    finalize_workflow(best_params)
  
  #######################################################
  ###### 2) FIT THE MODEL AND EVALUATE ON TEST DATA
  
  # fit the model and evaluate on the test data
  rf_fit <- final_rf_workflow %>%  last_fit(split = data_split)
  
  # predictions on the test set
  rf_fit_preds  <- rf_fit %>% collect_predictions()
  
  # get R2 from correlation
  rsq_rf_fit_preds <- rsq(rf_fit_preds, 
                          truth = !!sym(response_variable), 
                          estimate = .pred) %>% pull(.estimate)
  
  # plot observed vs predicted values
  
  if(response_variable == 'n_records') {
    plot_title = 'Number of records on iNaturalist'
  } 
  if(response_variable == 'p_research_grade'){
    plot_title = 'Proportion of records that reach Research Grade'
  }
  if(response_variable == 'n_users'){
    plot_title = 'Number of users recording'
  } 
  if(response_variable == 'n_species'){
    plot_title = 'Number of species recorded'
  }
  if(response_variable == 'n_projects'){
    plot_title = 'Number of projects created'
  }
  if(response_variable == 'n_literature'){
    plot_title = 'Number of papers published'
  }
  
  # plot observed vs predicted values
  plot_test_preds <- ggplot(rf_fit_preds, aes(!!sym(response_variable), .pred)) +
    geom_abline(lty = 2, color = 'orange', lwd=1) +
    geom_point(size = 2, alpha = 0.5, col = 'grey35') +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    labs(x = 'Observed', y = 'Predicted',
         title = str_glue('Number of records in GBIF'),
         subtitle = bquote(r == .(round(cor(rf_fit_preds$.pred, 
                                            rf_fit_preds[[response_variable]]),
                                        2)) ~ ', ' ~ R^2 == .(round(rsq_rf_fit_preds, 2)))) +
    coord_obs_pred() +
    ggpubr::theme_classic2()
  
  plot_test_preds <- ggplot(rf_fit_preds, aes(!!sym(response_variable), .pred)) +
    geom_abline(lty = 2, color = 'orange', lwd=1) +
    geom_point(size = 2, alpha = 0.5, col = 'grey35') +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    labs(x = 'Observed', y = 'Predicted',
         title = str_glue('Number of records in GBIF'),
         subtitle = bquote(r == .(round(cor(rf_fit_preds$.pred, 
                                            rf_fit_preds[[response_variable]]),
                                        2)) ~ ', ' ~ R^2 == .(round(rsq_rf_fit_preds, 2)))) +
    coord_obs_pred() +
    ggpubr::theme_classic2()
  
  # calculate variable importance (a property of the trained model - not of the test set)
  rf_model_importance <- rf_model %>%
    finalize_model(best_params) %>% 
    set_engine('ranger', 
               importance = 'impurity', # variable importance
               seed = 123)
  
  # generate dataset for plotting variable importance
  vip_df <- workflow() %>% 
    add_recipe(rf_recipe) %>% 
    add_model(rf_model_importance) %>% 
    fit(data_train) %>% 
    extract_fit_parsnip() %>%
    vi() %>%
    mutate(Category = case_when(Variable %in% c('gdp_per_capita', 'gdp_in_research') ~ 'money',
                                Variable %in% c('area', 'population') ~ 'structure',
                                Variable %in% c('latitude', 'iucn_species') ~ 'biodiversity',
                                Variable %in% c('has_node', 'neighbour_has_node') ~ 'network')) %>%
    mutate(standardise_Importance = Importance / sum(Importance)) %>% 
    mutate(standardise_Importance_R2 = (Importance * rsq_rf_fit_preds) / sum(Importance))
  
  # plot variable importance
  plot_vi <- ggplot(vip_df, aes(x = reorder(Variable, standardise_Importance_R2),
                                y = standardise_Importance_R2,
                                fill = Category)) +
    geom_col(show.legend = F) +
    coord_flip() +
    scale_fill_manual(values = c('money' = '#000000',
                                 'structure' = '#454545',
                                 'biodiversity' = '#bfbfbf',
                                 'network' = '#74ac00')) +
    geom_text(aes(label = scales::percent(standardise_Importance, accuracy = 1)),
              position = position_dodge(width = 1),    # move to center of bars
              hjust = 1.2,  col='white', #fontface = 'bold',
              size = 3) + 
    labs(
         y = expression('Importance (' * R^2 * ' proportion)'),
         x = '', fill = '',
         subtitle = 'Variable importance') +
    ggpubr::theme_cleveland() + 
    theme(panel.background = element_rect(fill ='grey99'),
          axis.line.x = element_line(colour = 'grey'))
  
  ###########################################################
  # generate dataset for partial dependence plot
  pd_df <- getDataForPartialPlot(vars = c(names(data_df)[-c(1:7)]),
                                 workflow = rf_fit,
                                 data = data_train) %>% 
    mutate(Category = case_when(Predictor %in% c('gdp_per_capita', 'gdp_in_research') ~ 'money',
                                Predictor %in% c('area', 'population') ~ 'structure',
                                Predictor %in% c('latitude', 'iucn_species') ~ 'biodiversity',
                                Predictor %in% c('has_node', 'neighbour_has_node') ~ 'network')) %>% 
    mutate(across(Predictor, ~factor(., 
                                     levels=c('gdp_per_capita',
                                              'gdp_in_research',
                                              'area',
                                              'population',
                                              'latitude',
                                              'iucn_species',
                                              'has_node',
                                              'neighbour_has_node'))))
  
  plot_pd <- ggplot(pd_df, aes(x, y, col=Category)) + 
    geom_line(lwd=1.25, alpha=0.8, show.legend = F) +
    scale_colour_manual(values = c('money' = '#000000',
                                   'structure' = '#454545',
                                   'biodiversity' = '#bfbfbf',
                                   'network' = '#74ac00')) +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    facet_wrap(~Predictor, scales = 'free_x') + 
    labs(y= plot_title, col = '',
         subtitle='Partial dependence') +
    ggpubr::theme_pubr() +
    theme(axis.title.x = element_blank())


  #######################################################
  ###### 3) CROSS VALIDATION
  
  
  # register the future backend with foreach (required by tidymodels)
  registerDoFuture()
  set.seed(1457, kind = "L'Ecuyer-CMRG")
  
  plan(multisession, workers = 8)
  
  # parallelise over resamples
  ctrl_resamples <- control_resamples(parallel_over = 'resamples',
                                      save_pred     = TRUE,
                                      verbose       = FALSE)
  
  start <- Sys.time()
  
  rf_final_cv <- final_rf_workflow %>% 
    fit_resamples(resamples = random_folds,
                  control   = ctrl_resamples)
  
  end <- Sys.time()
  print(end - start)
  
  plan(sequential)
  
  # collect_metrics(rf_final_cv)
  rf_final_cv_preds <- collect_predictions(rf_final_cv)
  
  rsq_rf_final_cv_preds <- rsq(rf_final_cv_preds, 
                                 truth = !!sym(response_variable), 
                                 estimate = .pred) %>% 
    pull(.estimate)
  
  # plot observed vs predicted
  plot_cv_preds <- ggplot(rf_final_cv_preds, aes(!!sym(response_variable), .pred)) +
    geom_abline(lty = 2, color = 'orange', lwd=1) +
    geom_point(size=2, alpha = 0.5, col = 'grey35') +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    labs(x = 'Observed', y = 'Predicted',
         title= str_glue('{plot_title} (CV)'),
         subtitle = bquote(r == .(round(cor(rf_final_cv_preds$.pred, 
                                            rf_final_cv_preds[[response_variable]]),
                                        2)) ~ ', ' ~ R^2 == .(round(rsq_rf_final_cv_preds, 2))),
         caption = paste0('Pearson correlation: ', round(cor(rf_final_cv_preds$.pred, 
                                                             rf_final_cv_preds[[response_variable]]), 3))) +
    coord_obs_pred() +
    ggpubr::theme_classic2()
  
  #######################################################
  ###### 4) RESULTS OF MODEL VALIDATION
  
  # rmse (RMSE): root mean square error
  # rsq (R2): squared correlation
  
  final_model_for_prod <- final_rf_workflow %>% fit(data = data_df)
  
  final_fit <- fit(final_rf_workflow, data_train) 
  train_preds <- predict(final_fit, data_train) %>%
    bind_cols(data_train)
  
  cv_rsq <- rsq(rf_final_cv_preds, truth = !!sym(response_variable), estimate = .pred)
  train_rsq <- rsq(train_preds, truth = !!sym(response_variable), estimate = .pred)
  test_rsq <- rsq(rf_fit_preds, truth = !!sym(response_variable), estimate = .pred)

  rsq_train_test <- bind_rows(train = train_rsq,
                              test  = test_rsq,
                              cv = cv_rsq,  
                              .id = "dataset")
  
  cv_rmse <- rmse(rf_final_cv_preds, truth = !!sym(response_variable), estimate = .pred)
  train_rmse <- rmse(train_preds, truth = !!sym(response_variable), estimate = .pred)
  test_rmse <- rmse(rf_fit_preds, truth = !!sym(response_variable), estimate = .pred)
  
  rmse_train_test <- bind_rows(train = train_rmse,
                               test  = test_rmse,
                               cv = cv_rmse,  
                               .id = "dataset")
  
  return(list(plot_tuning_params, plot_test_preds, plot_cv_preds,
              plot_vi, plot_pd,
              rsq_train_test, rmse_train_test, rsq_rf_final_cv_preds,
              vip_df, pd_df))
}


#' Get data for partial plots
#'
#' @param vars 
#' @param workflow 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
getDataForPartialPlot <- function(vars, workflow, data) {
  
  if(any(class(data) == 'sf')){
    data <- as_tibble(data)
  }
  
  map(vars, function(var) {
    partial_dep(extract_workflow(workflow), v = var, X = data)$data %>%
      setNames(c('x', 'y'))
  }) %>%
    bind_rows(.id = 'Predictor') %>%
    mutate(Predictor = vars[as.integer(Predictor)])
}
