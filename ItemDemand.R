#install.packages('tidyverse')
library(tidyverse)
#install.packages('tidymodels')
library(tidymodels)
#install.packages('DataExplorer')
#install.packages("poissonreg")
# library(poissonreg)
#install.packages("glmnet")
library(glmnet)
#library(patchwork)
# install.packages("rpart")
#install.packages('ranger')
library(ranger)
#install.packages('stacks')
library(stacks)
#install.packages('vroom')
library(vroom)
#install.packages('parsnip')
library(parsnip)
# install.packages('dbarts')
# library(dbarts)
#install.packages('embed')
library(embed)
library(themis)


############################### Time Series EDA ##############################

# Time Series Visualizations
#install.packages('timetk')
library(timetk)
library(gridExtra)
library(ggplot2)

data_train <- vroom("./data/train.csv")

# # Store 1 Item 1
# data_train_s1_i10 <- data_train %>%
#   filter(store == 1, item==10)
# # Store 2 Item 1
# data_train_s2_i10 <- data_train %>%
#   filter(store == 2, item==10)
# # Store 3 Item 1
# data_train_s3_i5 <- data_train %>%
#   filter(store == 3, item==5)
# # Store 4 Item 1
# data_train_s3_i25 <- data_train %>%
#   filter(store == 3, item==25)
# 
# data_train_s1_i1 %>%
#   plot_time_series(date, sales, .interactive = FALSE)
# 
# fig1 <- data_train_s1_i10 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max = 2*365)
# fig2 <- data_train_s2_i10 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max = 2*365)
# fig3 <- data_train_s3_i5 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max = 2*365)
# fig4 <- data_train_s3_i25 %>%
#   pull(sales) %>%
#   forecast::ggAcf(., lag.max = 2*365)
# 
# fig_grid <- grid.arrange(
#   arrangeGrob(fig1, top = "Store 1, Item 10"),
#   arrangeGrob(fig2, top = "Store 2, Item 10"),
#   arrangeGrob(fig3, top = "Store 3, Item 5"),
#   arrangeGrob(fig4, top = "Store 3, Item 25"),
#   ncol = 2
# )
  
# grid.arrange(fig1, fig2, fig3, fig4, ncol = 2)



########################### Time Series Prediction #############################

storeItem <- data_train %>%
  filter(store==9, item==36) %>%
  select(date, sales)

rFormula <- sales ~ .

storeItem_recipe <- recipe(rFormula, data = storeItem) %>% # set model formula and dataset
  step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
  step_holiday(date) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prepped_recipe <- prep(storeItem_recipe) # preprocessing new data
baked_data1 <- bake(prepped_recipe, new_data = storeItem)

rf_mod <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 800) %>% #Type of model
  set_engine('ranger') %>%
  set_mode('regression')

pretune_workflow <- workflow() %>%
  add_recipe(storeItem_recipe) %>%
  add_model(rf_mod)

## Grid of values to tune over
tuning_grid <- grid_regular(mtry(range = c(1,ncol(storeItem)-1)),
                            min_n(),
                            levels = 5) ## L^2 total tuning possibilities

# Split data for CV
folds <- vfold_cv(storeItem, v = 5, repeats = 1)

# Run CV
CV_results <- pretune_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

bestTune <- CV_results %>%
  select_best('smape')

collect_metrics(CV_results) %>%
  filter(mtry == bestTune$mtry, min_n == bestTune$min_n, .config == bestTune$.config) %>%
  pull(mean)



################ Time Series Prediction / CV / ####################

# For CV in Time Series: Hack off however much you wish to predict for (e.g. 1 yr, then 1 yr)

# install.packages('modeltime')
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(gridExtra)
library(plotly)

####################################
# CV Results: Time Series Estimation
####################################

train <- data_train %>% filter(store==3, item==17)
cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)
cv_split %>% 
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))
#######################
## Visualize CV results
#######################

fig1 <- cv_results %>%
  modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = train
                   ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


##################################
# Fitting CV results to our model:
##################################

## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = train)

data_test <- vroom("./data/test.csv")
test <- data_test %>% filter(store==3, item==17)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

fig3 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train) %>%
  plot_modeltime_forecast(.interactive=FALSE)

################################ Other store/item combo #########################################
# train2 <- data_train %>% filter(store==9, item==36)
# cv_split <- time_series_split(train2, assess="3 months", cumulative = TRUE)
# cv_split %>% 
#   tk_time_series_cv_plan() %>% #Put into a data frame
#   plot_time_series_cv_plan(date, sales, .interactive=FALSE)
# 
# es_model <- exp_smoothing() %>%
#   set_engine("ets") %>%
#   fit(sales~date, data=training(cv_split))
# 
# ## Cross-validate to tune model
# cv_results <- modeltime_calibrate(es_model,
#                                   new_data = testing(cv_split))

## Visualize CV results
# fig2 <- cv_results %>%
#   modeltime_forecast(
#     new_data = testing(cv_split),
#     actual_data = train2
#   ) %>%
#   plot_modeltime_forecast(.interactive=TRUE)
# 
# ## Evaluate the accuracy
# cv_results %>%
#   modeltime_accuracy() %>%
#   table_modeltime_accuracy(.interactive = FALSE)
# 
# 
# ## Refit to all data then forecast
# es_fullfit <- cv_results %>%
#   modeltime_refit(data = train2)
# 
# data_test <- vroom("./data/test.csv")
# test2 <- data_test %>% filter(store==9, item==36)
# 
# es_preds <- es_fullfit %>%
#   modeltime_forecast(h = "3 months") %>%
#   rename(date=.index, sales=.value) %>%
#   select(date, sales) %>%
#   full_join(., y=test2, by="date") %>%
#   select(id, sales)
# 
# fig4 <- es_fullfit %>%
#   modeltime_forecast(h = "3 months", actual_data = train2) %>%
#   plot_modeltime_forecast(.interactive=FALSE)
# 
# 
# 
# 
# fig_grid <- subplot(fig1, fig2, fig3, fig4, nrows = 2) %>%
#   layout(
#     title = 'Time Series Prediction',
#     annotations = list(
#       list(x = 0.2, y = 1, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE),
#       list(x = 0.8, y = 1, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE),
#       list(x = 0.2, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE),
#       list(x = 0.8, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE)
#     )
#   )
# 
# htmlwidgets::saveWidget(fig_grid, "ts_grid_plot.html")




#########################################################################
################## SARIMA (ARIMA) Models ################################
#########################################################################

library(forecast)
library(modeltime)

# First store item combo: #
storeItem1 <- data_train %>%
  filter(store==9, item==36) %>%
  select(date, sales)

# Second store item combo: #
storeItem2 <- data_train %>%
  filter(store==3, item==17) %>%
  select(date, sales)

rFormula <- sales ~ .

arima_recipe_1 <- recipe(rFormula, data = storeItem1) %>% # set model formula and dataset
  step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
  step_holiday(date) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

arima_recipe_2 <- recipe(rFormula, data = storeItem2) %>% # set model formula and dataset
  step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
  step_holiday(date) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prepped_recipe_1 <- prep(arima_recipe_1) # preprocessing new data
baked_data_arima1 <- bake(prepped_recipe_1, new_data = storeItem1)

prepped_recipe_2 <- prep(arima_recipe_2) # preprocessing new data
baked_data_arima2 <- bake(prepped_recipe_2, new_data = storeItem2)

arima_model1 <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2) %>% #default max D to tune
                         set_engine("auto_arima")

cv_split_1 <- time_series_split(storeItem1, assess="3 months", cumulative = TRUE)
cv_split_1 %>% 
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split_2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)
cv_split_2 %>% 
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_wf_1 <- workflow() %>%
  add_recipe(arima_recipe_1) %>%
  add_model(arima_model1) %>%
  fit(data=training(cv_split_1))

arima_wf_2 <- workflow() %>%
  add_recipe(arima_recipe_2) %>%
  add_model(arima_model1) %>%
  fit(data=training(cv_split_2))

cv_results_1 <- modeltime_calibrate(arima_wf_1,
                                  new_data = testing(cv_split_1))

cv_results_2 <- modeltime_calibrate(arima_wf_2,
                                    new_data = testing(cv_split_2))

train_1 <- data_train %>% filter(store==9, item==36)
train_2 <- data_train %>% filter(store==3, item==17)

##################################################
## Visualize CV results for first store item combo
##################################################

fig1 <- cv_results_1 %>%
  modeltime_forecast(
    new_data = testing(cv_split_1),
    actual_data = train_1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results_1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

##################################
# Fitting CV results to our model:
##################################

## Refit to all data then forecast
arima_fullfit <- cv_results_1 %>%
  modeltime_refit(data = train_1)

data_test <- vroom("./data/test.csv")
test_1 <- data_test %>% filter(store==9, item==36)

arima_preds_1 <- arima_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_1, by="date") %>%
  select(id, sales)

fig3 <- arima_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train_1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

###################################################
## Visualize CV results for second store item combo
###################################################

fig2 <- cv_results_2 %>%
  modeltime_forecast(
    new_data = testing(cv_split_2),
    actual_data = train_2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results_2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

##################################
# Fitting CV results to our model:
##################################

## Refit to all data then forecast
arima_fullfit_2 <- cv_results_2 %>%
  modeltime_refit(data = train_2)

test_2 <- data_test %>% filter(store==3, item==17)

arima_preds_2 <- arima_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_2, by="date") %>%
  select(id, sales)

fig4 <- arima_fullfit_2 %>%
  modeltime_forecast(h = "3 months", actual_data = train_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

fig_grid_arima <- subplot(fig1, fig2, fig3, fig4, nrows = 2) %>%
  layout(
    title = 'Time Series Prediction: Arima Models',
    annotations = list(
      list(x = 0.2, y = 1, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE),
      list(x = 0.8, y = 1, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE),
      list(x = 0.2, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE),
      list(x = 0.8, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE)
    )
  )

# Save grid:
# htmlwidgets::saveWidget(fig_grid_arima, "ts_arima_grid.html")


# **** #
# modeltime_forecast(new-data = itemTest)
# **** #

## Calibrate (i.e. tune) workflow
## Visualize & Evaluate CV accuracy
## Refit best model to entire data and predic




############################################################################
################## Facebook's Prophet Model ################################
############################################################################

library(forecast)
library(modeltime)
library(prophet)

# First store item combo: #
storeItem1 <- data_train %>%
  filter(store==9, item==36) %>%
  select(date, sales)

# Second store item combo: #
storeItem2 <- data_train %>%
  filter(store==3, item==17) %>%
  select(date, sales)

rFormula <- sales ~ .

prophet_recipe_1 <- recipe(rFormula, data = storeItem1) # %>% # set model formula and dataset
  # step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
  # step_holiday(date) %>%
  # step_range(date_doy, min = 0, max = pi) %>%
  # step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prophet_recipe_2 <- recipe(rFormula, data = storeItem2) # %>% # set model formula and dataset
  # step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
  # step_holiday(date) %>%
  # step_range(date_doy, min = 0, max = pi) %>%
  # step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prepped_recipe_1 <- prep(prophet_recipe_1) # preprocessing new data
baked_data_prophet1 <- bake(prepped_recipe_1, new_data = storeItem1)

prepped_recipe_2 <- prep(prophet_recipe_2) # preprocessing new data
baked_data_prophet2 <- bake(prepped_recipe_2, new_data = storeItem2)

cv_split_1 <- time_series_split(storeItem1, assess="3 months", cumulative = TRUE)
cv_split_1 %>% 
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

cv_split_2 <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)
cv_split_2 %>% 
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model_1 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split_1))

prophet_model_2 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split_2))

## Calibrate (i.e. tune) workflow
cv_results_1 <- modeltime_calibrate(prophet_model_1,
                                    new_data = testing(cv_split_1))

cv_results_2 <- modeltime_calibrate(prophet_model_2,
                                    new_data = testing(cv_split_2))

train_1 <- data_train %>% filter(store==9, item==36)
train_2 <- data_train %>% filter(store==3, item==17)


##################################################
## Visualize CV results for first store item combo
##################################################

fig1 <- cv_results_1 %>%
  modeltime_forecast(
    new_data = testing(cv_split_1),
    actual_data = train_1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results_1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

##################################
# Fitting CV results to our model:
##################################

## Refit to all data then forecast
prophet_fullfit <- cv_results_1 %>%
  modeltime_refit(data = train_1)

data_test <- vroom("./data/test.csv")
test_1 <- data_test %>% filter(store==9, item==36)

prophet_preds_1 <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_1, by="date") %>%
  select(id, sales)

fig3 <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train_1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

###################################################
## Visualize CV results for second store item combo
###################################################

fig2 <- cv_results_2 %>%
  modeltime_forecast(
    new_data = testing(cv_split_2),
    actual_data = train_2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results_2 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

##################################
# Fitting CV results to our model:
##################################

## Refit to all data then forecast
prophet_fullfit_2 <- cv_results_2 %>%
  modeltime_refit(data = train_2)

test_2 <- data_test %>% filter(store==3, item==17)

prophet_preds_2 <- prophet_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test_2, by="date") %>%
  select(id, sales)

fig4 <- prophet_fullfit_2 %>%
  modeltime_forecast(h = "3 months", actual_data = train_2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

fig_grid_prophet <- subplot(fig1, fig2, fig3, fig4, nrows = 2) %>%
  layout(
    title = 'Time Series Prediction: FB Prophet',
    annotations = list(
      list(x = 0.2, y = 1, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE),
      list(x = 0.8, y = 1, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE),
      list(x = 0.2, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 9, Item 36', showarrow = FALSE),
      list(x = 0.8, y = 0.45, xref = 'paper', yref = 'paper', text = 'Store 3, Item 17', showarrow = FALSE)
    )
  )


# Save grid:
# htmlwidgets::saveWidget(fig_grid_prophet, "ts_prophet_grid.html")



###########################################################################
################## Loop through all stores ################################
###########################################################################

library(forecast)
library(modeltime)
library(prophet)

data_test <- vroom("./data/test.csv")

rFormula <- sales ~ .

nStores <- max(data_train$store)
nItems <- max(data_train$item)

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- data_train %>%
      filter(store==s, item==i)
    storeItemTest <- data_test %>%
      filter(store==s, item==i)

    ## Fit storeItem models here
    prophet_recipe <- recipe(rFormula, data = storeItemTrain) %>% # set model formula and dataset
      step_date(date, features = c('dow', 'doy', 'week', 'month', 'year', 'decimal')) %>%
      step_holiday(date) %>%
      step_range(date_doy, min = 0, max = pi) %>%
      step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
    
    prepped_recipe <- prep(prophet_recipe) # preprocessing new data
    baked_data_prophet <- bake(prepped_recipe, new_data = storeItemTrain)
    
    cv_split_1 <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)
    cv_split_1 %>% 
      tk_time_series_cv_plan() %>% #Put into a data frame
      plot_time_series_cv_plan(date, sales, .interactive=FALSE)
    
    prophet_model_1 <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(cv_split_1))

    cv_results_1 <- modeltime_calibrate(prophet_model_1,
                                        new_data = testing(cv_split_1))
    
    ## Evaluate the accuracy
    cv_results_1 %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(.interactive = FALSE)
    
    
    ##################################
    # Fitting CV results to our model:
    ##################################
    
    ## Refit to all data then forecast
    prophet_fullfit <- cv_results_1 %>%
      modeltime_refit(data = storeItemTrain)
    
    prophet_preds_1 <- prophet_fullfit %>%
      modeltime_forecast(h = "3 months") %>%
      rename(date=.index, sales=.value) %>%
      select(date, sales) %>%
      full_join(., y=storeItemTest, by="date") %>%
      select(id, sales)
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- prophet_preds_1
      } 
    else {
      all_preds <- bind_rows(all_preds, prophet_preds_1)
    }
    
  }
}

vroom_write(all_preds, "./data/ItemDemand_preds.csv", delim = ",")
















