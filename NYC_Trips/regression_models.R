# Set workspace and import dataset
setwd("/Users/advaitiyer/Desktop/")
df <- read.csv("train1.csv",sep = '\t', encoding='utf-8')
# Sampling
df$X <- NULL
df$delay_class <- NULL
df1 <- df
df1$mrn_peak <- NULL
df1$eve_peak <- NULL
df1$day_Monday <- NULL
df1$day_Tuesday <- NULL
df1$day_Wednesday <- NULL
df1$day_Thursday <- NULL
df1$day_Saturday <- NULL
df1$day_Sunday <- NULL
df1$hour_1.0 <- NULL
df1$hour_2.0 <- NULL
df1$hour_3.0 <- NULL
df1$hour_4.0 <- NULL
df1$hour_5.0 <- NULL
df1$hour_6.0 <- NULL
df1$hour_7.0 <- NULL
df1$hour_8.0 <- NULL
df1$hour_9.0 <- NULL
df1$hour_10.0 <- NULL
df1$hour_11.0 <- NULL
df1$hour_12.0 <- NULL
df1$hour_13.0 <- NULL
df1$hour_14.0 <- NULL
df1$hour_15.0 <- NULL
df1$hour_16.0 <- NULL
df1$hour_17.0 <- NULL
df1$hour_18.0 <- NULL
df1$hour_19.0 <- NULL
df1$hour_20.0 <- NULL
df1$hour_21.0 <- NULL
df1$hour_22.0 <- NULL
df1$hour_23.0 <- NULL
df1$month_2.0 <- NULL
df1$month_3.0 <- NULL
df1$month_4.0 <- NULL
df1$month_5.0 <- NULL
df1$month_6.0 <- NULL
df1$month_7.0 <- NULL
df1$vendor_2.0 <- NULL


library(corrplot)
library(ggcorrplot)

corr <- cor(df1)
plt1 <- ggcorrplot(corr, outline.col = "white",
                   ggtheme = ggplot2::theme_gray,
                   colors = c("#6D9EC1", "white", "#E46726"), sig.level=0.05,lab_size = 4.5, p.mat = NULL,
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 20) +
  theme(axis.text.x = element_text(size=13,angle=90,margin=margin(-2,0,0,0)),
        axis.text.y = element_text(size=13,margin=margin(0,-2,0,0)),
        panel.grid.minor = element_line(size=10)) + 
  geom_tile(fill="lightgrey") +
  geom_tile(height=0.9, width=0.9)
plt1
df$pickup_latitude <- NULL
df$dropoff_latitude <- NULL
df$pickup_longitude <- NULL
df$dropoff_longitude <- NULL
df$speed_lag <- NULL
df$mrn_peak <- NULL
df$eve_peak <- NULL

#df$mrn_peak <- as.factor(df$mrn_peak)
#df$eve_peak <- as.factor(df$eve_peak)
df$accident <- as.factor(df$accident)
df$day_Monday <- as.factor(df$day_Monday)
df$day_Tuesday <- as.factor(df$day_Tuesday)
df$day_Wednesday <- as.factor(df$day_Wednesday)
df$day_Thursday <- as.factor(df$day_Thursday)
df$day_Saturday <- as.factor(df$day_Saturday)
df$day_Sunday <- as.factor(df$day_Saturday)
df$hour_1.0 <- as.factor(df$hour_1.0)
df$hour_2.0 <- as.factor(df$hour_2.0)
df$hour_3.0 <- as.factor(df$hour_3.0)
df$hour_4.0 <- as.factor(df$hour_4.0)
df$hour_5.0 <- as.factor(df$hour_5.0)
df$hour_6.0 <- as.factor(df$hour_6.0)
df$hour_7.0 <- as.factor(df$hour_7.0)
df$hour_8.0 <- as.factor(df$hour_8.0)
df$hour_9.0 <- as.factor(df$hour_9.0)
df$hour_10.0 <- as.factor(df$hour_10.0)
df$hour_11.0 <- as.factor(df$hour_11.0)
df$hour_12.0 <- as.factor(df$hour_12.0)
df$hour_13.0 <- as.factor(df$hour_13.0)
df$hour_14.0 <- as.factor(df$hour_14.0)
df$hour_15.0 <- as.factor(df$hour_15.0)
df$hour_16.0 <- as.factor(df$hour_16.0)
df$hour_17.0 <- as.factor(df$hour_17.0)
df$hour_18.0 <- as.factor(df$hour_18.0)
df$hour_19.0 <- as.factor(df$hour_19.0)
df$hour_20.0 <- as.factor(df$hour_20.0)
df$hour_21.0 <- as.factor(df$hour_21.0)
df$hour_22.0 <- as.factor(df$hour_22.0)
df$hour_23.0 <- as.factor(df$hour_23.0)
df$month_2.0 <- as.factor(df$month_2.0)
df$month_3.0 <- as.factor(df$month_3.0)
df$month_4.0 <- as.factor(df$month_4.0)
df$month_5.0 <- as.factor(df$month_5.0)
df$month_6.0 <- as.factor(df$month_6.0)
df$month_7.0 <- as.factor(df$month_7.0)
df$vendor_2.0 <- as.factor(df$vendor_2.0)

# Linear Regression
linear_reg <- lm(df$trip_duration~., data = df)
summary(linear_reg) # 0.4887
df2 <- df
df <- df2

df$precip <- NULL
df$snow_fall <- NULL
#df$accident <- NULL
#df$vendor_2.0 <- NULL

linear_reg1 <- lm(df$trip_duration~., data = df)
summary(linear_reg1)

linear_reg2 <- lm(df1$trip_duration~., data=df1)
summary(linear_reg2)

df$dropoff_longitude <- NULL
df$pickup_latitude <- NULL
df$dropoff_latitude <- NULL
df$pickup_longitude <- NULL
df$mrn_peak<- NULL
df$eve_peak<-NULL
df$speed_lag<- NULL
df$snow_fall<- NULL

df_wd<- df

linear_reg3 <- lm(df1$trip_duration~., data=df)
w_dummy<-summary(linear_reg3)

df1$speed_lag<-NULL
linear_reg4 <- lm(df1$trip_duration~., data=df1)
summary(linear_reg4)

library(MASS)
library(caret)

step <- stepAIC(linear_reg1, direction="both")
step$anova # display results
summary(step)

library(tidyverse)
library(caret)
#install.packages('glmnet')
library(glmnet)
#install.packages("glmnetUtils")
library(glmnetUtils)

# Split the data into training and test set
set.seed(123)
training.samples <- df$trip_duration %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

x <- train.data
y <- train.data$trip_duration
x$trip_duration <- NULL

x_test <- test.data
y_test <- test.data$trip_duration
x_test$trip_duration <- NULL

# Ridge regression
set.seed(1)
ridge_mod = cv.glmnet(data.matrix(x), y, alpha = 0) # Fit lasso model on training data
plot(ridge_mod) # Draw plot of training MSE as a function of lambda
bestlam_ridge = ridge_mod$lambda.min # Select lamda that minimizes training MSE
ridge_pred = predict(ridge_mod, s = bestlam_ridge, newx = data.matrix(x_test)) # Use best lambda to predict test data
rmse_ridge <- (mean((ridge_pred - y_test)^2))^(1/2) # Calculate test RMSE
rmse_ridge

# Lasso regression
set.seed(1)
lasso_mod = cv.glmnet(data.matrix(x), y, alpha = 1) # Fit lasso model on training data
plot(lasso_mod) # Draw plot of training MSE as a function of lambda
bestlam_lasso = lasso_mod$lambda.min # Select lamda that minimizes training MSE
lasso_best = glmnet(data.matrix(x), y, alpha = 1, lambda = bestlam_lasso)
lasso_pred = predict(lasso_mod, s = bestlam_lasso, newx = data.matrix(x_test)) # Use best lambda to predict test data
lasso_best_pred = predict(lasso_best, s = bestlam_lasso, newx = data.matrix(x_test))
rmse_lasso <- (mean((lasso_pred - y_test)^2))^(1/2) # Calculate test RMSE
rmse_best_lasso <- (mean((lasso_best_pred - y_test)^2))^(1/2) # Calculate test RMSE
rmse_best_lasso
coef(lasso_best)

# Elastic net regularized regression
library(caret)
set.seed(123)
elasticnet_mod <- train(
  trip_duration~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
elasticnet_mod$bestTune
# Coefficient of the final model. You need
# to specify the best lambda
coef(elasticnet_mod$finalModel, elasticnet_mod$bestTune$lambda)
# Make predictions
elasticnet_mod_predictions <- elasticnet_mod %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(elasticnet_mod_predictions, test.data$trip_duration),
  Rsquare = R2(elasticnet_mod_predictions, test.data$trip_duration)
)
plot(elasticnet_mod)

models <- list(ridge=ridge_mod, lasso_mod, elasticnet_mod)
