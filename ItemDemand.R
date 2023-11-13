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

# Store 1 Item 1
data_train_s1_i10 <- data_train %>%
  filter(store == 1, item==10)
# Store 2 Item 1
data_train_s2_i10 <- data_train %>%
  filter(store == 2, item==10)
# Store 3 Item 1
data_train_s3_i5 <- data_train %>%
  filter(store == 3, item==5)
# Store 4 Item 1
data_train_s3_i25 <- data_train %>%
  filter(store == 3, item==25)

data_train_s1_i1 %>%
  plot_time_series(date, sales, .interactive = FALSE)

fig1 <- data_train_s1_i10 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365)
fig2 <- data_train_s2_i10 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365)
fig3 <- data_train_s3_i5 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365)
fig4 <- data_train_s3_i25 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 2*365)

fig_grid <- grid.arrange(
  arrangeGrob(fig1, top = "Store 1, Item 10"),
  arrangeGrob(fig2, top = "Store 2, Item 10"),
  arrangeGrob(fig3, top = "Store 3, Item 5"),
  arrangeGrob(fig4, top = "Store 3, Item 25"),
  ncol = 2
)
  
# grid.arrange(fig1, fig2, fig3, fig4, ncol = 2)





