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
model_iNat_RF <- function(data_df, response_variable, skip_vars) {
  # random forest 
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
    step_normalize(all_predictors())
  
  # define model
  rf_model <- rand_forest(trees = 1000, 
                          min_n = tune(), # to figure out the right value
                          mtry = tune()   # to figure out the right value
  ) %>%
    set_mode('regression') %>%
    set_engine('ranger')
  
  # workflow
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model) 
  
  ### create random folds for tunning
  set.seed(234)
  random_folds <- vfold_cv(data_train, v = 5)
  
  # num of predictors in the data minus siteID, geometry and the explanatory variable
  n_mtry <- length(data_train)-3 
  
  # grid for tunning
  rf_grid <- grid_regular(
    mtry(range = c(1, n_mtry)), # how many predictors are taken
    min_n(range = c(2,20)) # minimal node size
  )
  
  # tune model on random blocks
  set.seed(12345)
  doParallel::registerDoParallel()
  start <- Sys.time()
  
  rf_tune <- tune_grid(rf_workflow, 
                       resamples = random_folds, 
                       grid = rf_grid)
  end <- Sys.time()
  print(end-start)
  
  # visualize tuning result
  plot_tuning_params <- autoplot(rf_tune) + 
    theme_bw() +
    ggtitle('Tuning results (trees=1000)')
  
  # show the best hyperparameters
  # show_best(rf_tune, metric = 'rsq')
  
  # select the best parameters
  best_params <- rf_tune %>% select_best(metric = 'rmse')
  
  # finalise workflow with the best parameters
  final_rf_workflow <- rf_workflow %>% 
    finalize_workflow(best_params)
  
  #######################################################
  
  ###### 2) FIT THE MODEL AND EVALUATE ON TEST DATA
  
  # last fit (we are fitting our model to the whole training data, and evaluating in the testing data)
  rf_fit <- last_fit(final_rf_workflow, data_split)
  
  # predictions on the test set
  rf_fit_preds  <- collect_predictions(rf_fit)
  
  # get R2 from correlation
  rsq_rf_fit_preds <- round((cor(rf_fit_preds$.pred, rf_fit_preds[[response_variable]]))^2, 3)
  
  # plot observed vs predicted values
  
  if(response_variable == 'n_records_inat') {
    plot_title = 'Number of records on iNaturalist'
  } 
  if(response_variable == 'p_gbif'){
    plot_title = 'Proportion of records from iNaturalist on GBIF'
  }
  if(response_variable == 'n_users'){
    plot_title = 'Number of users recording'
  } 
  if(response_variable == 'n_taxa'){
    plot_title = 'Number of taxa recorded'
  }
  
  plot_test_preds <- ggplot(rf_fit_preds, aes(!!sym(response_variable), .pred)) +
    geom_abline(lty = 2, color = 'orange', lwd=1) +
    geom_point(size=2, alpha = 0.5, col = 'grey35') +
    scale_x_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = c(m = 1000000))) +
    labs(x = 'Observed', y = 'Predicted',
         title= plot_title,
         subtitle = bquote(r == .(round(cor(rf_fit_preds$.pred, 
                                            rf_fit_preds[[response_variable]]),
                                        2)) ~ ", " ~ R^2 == .(round(rsq_rf_fit_preds, 2))),
         paste0('Pearson correlation: ', round(cor(rf_fit_preds$.pred, 
                                                   rf_fit_preds[[response_variable]]), 3))) +
    coord_fixed() +
    ggpubr::theme_classic2()
  
  # calculate variable importance
  importatnce_rf_model <- rf_model %>%
    finalize_model(best_params) %>% 
    set_engine('ranger', 
               importance = 'impurity', # variable importance
               seed = 1975)
  
  # generate dataset for plotting variable importance
  vip_df <- workflow() %>% 
    add_recipe(rf_recipe) %>% 
    add_model(importatnce_rf_model) %>% 
    fit(data_train) %>% 
    extract_fit_parsnip() %>%
    vi() %>%
    mutate(Category = case_when(Variable %in% c('gdp_per_capita', 'gdp_in_research') ~ 'money',
                                Variable %in% c('area', 'population') ~ 'structure',
                                Variable %in% c('latitude') ~ 'biodiversity',
                                Variable %in% c('has_site') ~ 'network')) %>%
    mutate(standardise_Importance = (Importance * rsq_rf_fit_preds) / sum(Importance))
  
  # plot variable importance
  plot_vi <- ggplot(vip_df, aes(x = reorder(Variable, standardise_Importance),
                                y = standardise_Importance,
                                fill = Category)) +
    geom_col(show.legend = F) +
    coord_flip() +
    scale_fill_manual(values = c('money' = '#000000',
                                 'structure' = '#454545',
                                 'biodiversity' = '#bfbfbf',
                                 'network' = '#74ac00')) +
    labs(title = plot_title,
         y = expression('Importance (' * R^2 * ' proportion)'),
         x = '', fill = '',
         subtitle = 'Variable importance') +
    theme_minimal()
  
  ###########################################################
  
  # generate dataset for partial dependence plot
  pd_df <- getDataForPartialPlot(vars = c(names(data_df)[-c(1,2,3,4,5)]),
                                 workflow = rf_fit,
                                 data = data_train) %>% 
    mutate(Category = case_when(Predictor %in% c('gdp_per_capita', 'gdp_in_research') ~ 'money',
                                Predictor %in% c('area', 'population') ~ 'structure',
                                Predictor %in% c('latitude') ~ 'biodiversity',
                                Predictor %in% c('has_site') ~ 'network')) %>% 
    mutate(across(Predictor, ~factor(., 
                                     levels=c('gdp_per_capita',
                                              'gdp_in_research',
                                              'area',
                                              'population',
                                              'latitude',
                                              'has_site'))))
  
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
  # 
  # #######################################################
  # ###### 3) SPATIAL CROSS-VALIDATION
  # 
  # set.seed(1457)
  # 
  # doParallel::registerDoParallel()
  # start <- Sys.time()
  # 
  # # fit resamples
  # rf_cv_spatial <- fit_resamples(final_rf_workflow,
  #                                resamples = spatial_folds,
  #                                control = control_resamples(save_pred = TRUE, save_workflow = TRUE))
  # 
  # end <- Sys.time()
  # print(end-start)
  # 
  # # collect predictions on fitted resamples
  # rf_cv_spatial_preds <- collect_predictions(rf_cv_spatial)
  # rsq_rf_cv_spatial_preds <- round((cor(rf_cv_spatial_preds$.pred, 
  #                                       rf_cv_spatial_preds[[response_variable]]))^2, 3)
  # 
  # 
  # if(response_variable == 'richness_change') {
  #   plot_title_cv = 'Change in species richness (cross-validation)'
  # } else {
  #   plot_title_cv = 'Temporal turnover (cross-validation)'
  # }
  # 
  # # plot observed vs predicted
  # plot_cv_spatial_preds <- ggplot(rf_cv_spatial_preds, aes(!!sym(response_variable), .pred)) +
  #   geom_abline(lty = 2, color = 'orange', lwd=1) +
  #   geom_point(size=2, alpha = 0.5, col = 'grey35') +
  #   labs(x = 'Observed', y = 'Predicted',
  #        title= plot_title_cv,
  #        subtitle = bquote(r == .(round(cor(rf_cv_spatial_preds$.pred, 
  #                                           rf_cv_spatial_preds[[response_variable]]),
  #                                       2)) ~ ", " ~ R^2 == .(round(rsq_rf_cv_spatial_preds, 2))),
  #        paste0('Pearson correlation: ', round(cor(rf_cv_spatial_preds$.pred, 
  #                                                  rf_cv_spatial_preds[[response_variable]]), 3))) +
  #   coord_fixed() +
  #   ggpubr::theme_classic2()
  # 
  
  #######################################################
  ###### 4) RESULTS OF MODEL VALIDATION
  # RMSE: root mean square error
  # rsq: squared correlation
  # cor: Pearson correlation of observed versus predicted values
  
  final_fit <- fit(final_rf_workflow, data_train) 
  train_preds <- predict(final_fit, data_train) %>%
    bind_cols(data_train)
  
  train_rsq <- rsq(train_preds, truth = !!sym(response_variable), estimate = .pred)
  test_rsq <- rsq(rf_fit_preds, truth = !!sym(response_variable), estimate = .pred)
  
  rsq_train_test <- bind_rows(train = train_rsq,
                              test  = test_rsq,
                              .id = "dataset")
  
  return(list(plot_tuning_params, plot_test_preds, 
              plot_vi, plot_pd,
              rsq_train_test, vip_df, pd_df))
  
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