Predictive-modeling
================
Tanishq Daniel
2024-03-12

``` r
library(tidymodels)  
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.1.1 ──

    ## ✔ broom        1.0.5     ✔ recipes      1.0.8
    ## ✔ dials        1.2.0     ✔ rsample      1.2.0
    ## ✔ dplyr        1.1.4     ✔ tibble       3.2.1
    ## ✔ ggplot2      3.4.4     ✔ tidyr        1.3.0
    ## ✔ infer        1.0.5     ✔ tune         1.1.2
    ## ✔ modeldata    1.2.0     ✔ workflows    1.1.3
    ## ✔ parsnip      1.1.1     ✔ workflowsets 1.0.1
    ## ✔ purrr        1.0.2     ✔ yardstick    1.2.0

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ purrr::discard() masks scales::discard()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ✖ recipes::step()  masks stats::step()
    ## • Learn how to get started at https://www.tidymodels.org/start/

``` r
# Helper packages
library(readr)       # for importing data
```

    ## 
    ## Attaching package: 'readr'

    ## The following object is masked from 'package:yardstick':
    ## 
    ##     spec

    ## The following object is masked from 'package:scales':
    ## 
    ##     col_factor

``` r
library(vip)         # for variable importance plots
```

    ## 
    ## Attaching package: 'vip'

    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
hotels <- 
  read_csv("https://tidymodels.org/start/case-study/hotels.csv") %>%
  mutate(across(where(is.character), as.factor))
```

    ## Rows: 50000 Columns: 23

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): hotel, children, meal, country, market_segment, distribution_chan...
    ## dbl  (11): lead_time, stays_in_weekend_nights, stays_in_week_nights, adults,...
    ## date  (1): arrival_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(hotels)
```

    ## [1] 50000    23

``` r
glimpse(hotels)
```

    ## Rows: 50,000
    ## Columns: 23
    ## $ hotel                          <fct> City_Hotel, City_Hotel, Resort_Hotel, R…
    ## $ lead_time                      <dbl> 217, 2, 95, 143, 136, 67, 47, 56, 80, 6…
    ## $ stays_in_weekend_nights        <dbl> 1, 0, 2, 2, 1, 2, 0, 0, 0, 2, 1, 0, 1, …
    ## $ stays_in_week_nights           <dbl> 3, 1, 5, 6, 4, 2, 2, 3, 4, 2, 2, 1, 2, …
    ## $ adults                         <dbl> 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 1, 2, …
    ## $ children                       <fct> none, none, none, none, none, none, chi…
    ## $ meal                           <fct> BB, BB, BB, HB, HB, SC, BB, BB, BB, BB,…
    ## $ country                        <fct> DEU, PRT, GBR, ROU, PRT, GBR, ESP, ESP,…
    ## $ market_segment                 <fct> Offline_TA/TO, Direct, Online_TA, Onlin…
    ## $ distribution_channel           <fct> TA/TO, Direct, TA/TO, TA/TO, Direct, TA…
    ## $ is_repeated_guest              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ previous_cancellations         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ previous_bookings_not_canceled <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ reserved_room_type             <fct> A, D, A, A, F, A, C, B, D, A, A, D, A, …
    ## $ assigned_room_type             <fct> A, K, A, A, F, A, C, A, D, A, D, D, A, …
    ## $ booking_changes                <dbl> 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ deposit_type                   <fct> No_Deposit, No_Deposit, No_Deposit, No_…
    ## $ days_in_waiting_list           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ customer_type                  <fct> Transient-Party, Transient, Transient, …
    ## $ average_daily_rate             <dbl> 80.75, 170.00, 8.00, 81.00, 157.60, 49.…
    ## $ required_car_parking_spaces    <fct> none, none, none, none, none, none, non…
    ## $ total_of_special_requests      <dbl> 1, 3, 2, 1, 4, 1, 1, 1, 1, 1, 0, 1, 0, …
    ## $ arrival_date                   <date> 2016-09-01, 2017-08-25, 2016-11-19, 20…

``` r
hotels %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))
```

    ## # A tibble: 2 × 3
    ##   children     n   prop
    ##   <fct>    <int>  <dbl>
    ## 1 children  4038 0.0808
    ## 2 none     45962 0.919

``` r
set.seed(91200)
splits      <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

# training set proportions by children
hotel_other %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))
```

    ## # A tibble: 2 × 3
    ##   children     n   prop
    ##   <fct>    <int>  <dbl>
    ## 1 children  3033 0.0809
    ## 2 none     34467 0.919

``` r
# test set proportions by children
hotel_test  %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))
```

    ## # A tibble: 2 × 3
    ##   children     n   prop
    ##   <fct>    <int>  <dbl>
    ## 1 children  1005 0.0804
    ## 2 none     11495 0.920

``` r
set.seed(91200)
val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)
```

    ## Warning: `validation_split()` was deprecated in rsample 1.2.0.
    ## ℹ Please use `initial_validation_split()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
val_set
```

    ## # Validation Set Split (0.8/0.2)  using stratification 
    ## # A tibble: 1 × 2
    ##   splits               id        
    ##   <list>               <chr>     
    ## 1 <split [30000/7500]> validation

``` r
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")
```

``` r
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())
```

``` r
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)
```

``` r
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values
```

    ## Selecting by penalty

    ## # A tibble: 5 × 1
    ##    penalty
    ##      <dbl>
    ## 1 0.0001  
    ## 2 0.000127
    ## 3 0.000161
    ## 4 0.000204
    ## 5 0.000259

``` r
lr_reg_grid %>% top_n(5)  # highest penalty values
```

    ## Selecting by penalty

    ## # A tibble: 5 × 1
    ##   penalty
    ##     <dbl>
    ## 1  0.0386
    ## 2  0.0489
    ## 3  0.0621
    ## 4  0.0788
    ## 5  0.1

``` r
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 
```

![](Predictive-Modeling_files/figure-gfm/tune%20model-1.png)<!-- -->

``` r
top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models
```

    ## # A tibble: 15 × 7
    ##     penalty .metric .estimator  mean     n std_err .config              
    ##       <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
    ##  1 0.0001   roc_auc binary     0.861     1      NA Preprocessor1_Model01
    ##  2 0.000127 roc_auc binary     0.862     1      NA Preprocessor1_Model02
    ##  3 0.000161 roc_auc binary     0.862     1      NA Preprocessor1_Model03
    ##  4 0.000204 roc_auc binary     0.862     1      NA Preprocessor1_Model04
    ##  5 0.000259 roc_auc binary     0.862     1      NA Preprocessor1_Model05
    ##  6 0.000329 roc_auc binary     0.863     1      NA Preprocessor1_Model06
    ##  7 0.000418 roc_auc binary     0.863     1      NA Preprocessor1_Model07
    ##  8 0.000530 roc_auc binary     0.863     1      NA Preprocessor1_Model08
    ##  9 0.000672 roc_auc binary     0.863     1      NA Preprocessor1_Model09
    ## 10 0.000853 roc_auc binary     0.863     1      NA Preprocessor1_Model10
    ## 11 0.00108  roc_auc binary     0.864     1      NA Preprocessor1_Model11
    ## 12 0.00137  roc_auc binary     0.864     1      NA Preprocessor1_Model12
    ## 13 0.00174  roc_auc binary     0.864     1      NA Preprocessor1_Model13
    ## 14 0.00221  roc_auc binary     0.863     1      NA Preprocessor1_Model14
    ## 15 0.00281  roc_auc binary     0.862     1      NA Preprocessor1_Model15

``` r
lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best
```

    ## # A tibble: 1 × 7
    ##   penalty .metric .estimator  mean     n std_err .config              
    ##     <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
    ## 1 0.00137 roc_auc binary     0.864     1      NA Preprocessor1_Model12

``` r
lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)
```

![](Predictive-Modeling_files/figure-gfm/tune%20model-2.png)<!-- -->
