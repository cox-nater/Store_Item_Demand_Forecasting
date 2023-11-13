library(tidymodels)
library(vroom)
library(tidyverse)
library(embed)
library(rsample)
library(timetk)
library(patchwork)

SIDF_train <- vroom('C:/BYU/2023(5) Fall/STAT 348/Store_Item_Demand_Forecasting/train.csv',
                   show_col_types = FALSE)
SIDF_test <- vroom('C:/BYU/2023(5) Fall/STAT 348/Store_Item_Demand_Forecasting/test.csv',
                  show_col_types = FALSE)

nStores <- max(SIDF_train$store)
nItems <- max(SIDF_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- SIDF_train %>%
      filter(store==s, item==i)
    storeItemTest <- SIDF_test %>%
      filter(store==s, item==i)
    #Fit storeITem models
    ##Predict StoreItem sales
    
    ##Save storeItem predictions
    if(s==1 & i ==1) {
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

storeItemTrain %>%
  plot_time_series(date, sales, .interactive=FALSE)

storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(.)

storeItemTrain %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

plot1 <- SIDF_train %>%
  filter(store == 3, item == 3) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot2 <- SIDF_train %>%
  filter(store == 10, item == 50) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot3 <- SIDF_train %>%
  filter(store == 8, item == 26) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot4 <- SIDF_train %>%
  filter(store == 8, item == 3) %>%
  pull(sales) %>%
  forecast::ggAcf(.)

(plot1 + plot2)/(plot3 + plot4)
