###____________________________________________________________
### Forecasting church attendance monthly, weekly.
###____________________________________________________________

setwd("C:/Users/ndfos/Desktop/")

install.packages(c("tidyverse", "WVPlots", "vtreat"))
library(tidyverse)
library(lubridate)
library(ggthemes)
library(broom)
library(ggfortify)
library(WVPlots)
library(vtreat)
library(mgcv)
install.packages("ranger")
library(ranger)

# Get file in memory.
Church_attendance <- read_csv(
  "attendance_2019_2023.csv",
  col_types = c("?", "n", "n", "n", "n", "c") # date format did not work on import
) %>% 
  mutate(date = mdy(date), # fix the date using lubridate
         year = as.character(year(date)),
         month = month(date, abbr = TRUE, label = TRUE),
         day = day(date), 
         week = week(date),
         .after = date
  ) %>% 
  mutate(cumavg = cummean(attendance),
         .after = attendance
  )

Church_attendance_filter <- Church_attendance %>% 
  filter(date >= "2020-11-08") # for modeling due to extreme changes in 2019-2020

#____________________________________________________________
# Draw graphs of attendance data distribution
#____________________________________________________________

ggplot(Church_attendance, aes(x = attendance)) + 
  geom_histogram(bins = 26) + 
  labs(title = "Distribution of Church Attendance 2019-2023",
       y = "Count",
       caption = "Dr. Nicolas Foss, MS") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("distribution_2019_2023.png", width = 6.5, height = 5, units = "in")

# density plot.

ggplot(Church_attendance, 
         aes(
           x = attendance
         )) + 
  geom_density(color = "red",
               linewidth = 2.5) + 
  labs(title = "Distribution of Church Attendance 2019-2023",
       y = "Count",
       caption = "Dr. Nicolas Foss, MS") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("density_2019_2023.png", width = 7.25, height = 5, units = "in")

# boxplot.

ggplot(Church_attendance, 
         aes(
           x = year, y = attendance
         )) +
  geom_boxplot(outlier.size = 0, alpha = 0.2) + 
  geom_point(color = "darkred", alpha = 0.2, position = "jitter") +
  labs(title = "Church Attendance 2019-2023",
       subtitle = "Comparisons between years",
       y = "Count",
       caption = "Dr. Nicolas Foss, MS") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("boxplot_2019_2023.png", width = 8, height = 5, units = "in")

# plot the cumulative average 2019 - 2023

Church_attendance %>% 
  ggplot(aes(
    x = date,
    y = cumavg
  )
  ) +
  geom_line(color = "darkorange",
            linewidth = 1.25,
            arrow = arrow(length = unit(1, "cm"))
  ) + 
  labs(title = "Cumulative Average Attendance",
       subtitle = "Years 2019 - 2023",
       x = "Date",
       y = "Cumulative Average Attendance",
       caption = "Dr. Nicolas Foss, MS"
  ) + 
  theme_gdocs()

ggsave("cumavg_2019_2023.png", width = 8, height = 5, units = "in")

# plot the cumulative average 2022 - 2023

Church_attendance %>% 
  filter(year %in% c("2022", "2023")) %>% 
  ggplot(aes(
    x = date,
    y = cumavg
  )
  ) +
  geom_line(color = "steelblue",
            linewidth = 2,
            arrow = arrow(length = unit(.5, "cm"))
  ) + 
  labs(title = "Cumulative Average Attendance",
       subtitle = "Years 2019 - 2023",
       x = "Date",
       y = "Cumulative Average Attendance",
       caption = "Dr. Nicolas Foss, MS"
  ) + 
  theme_gdocs()

ggsave("cumavg_2022_2023.png", width = 10, height = 6, units = "in")


#____________________________________________________________
# Plot scatterplot of attendance data for all years and months.
#____________________________________________________________

ggplot(Church_attendance, 
         aes(
           x = date,
           y = attendance
         )
  ) +
  geom_point(alpha = 0.5,
             color = "orange",
             size = 2,
             shape = 15) + 
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.75,
    color = "steelblue",
    alpha = 0.5
  ) + 
  labs(title = "Distribution of Church Attendance 2019-2023",
       y = "Count",
       caption = "Dr. Nicolas Foss, MS") + 
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("scatterplot_1.png", width = 6.5, height = 5, units = "in")

#_______________________________________________________________________________
# Fit regression models to the data.
#_______________________________________________________________________________

# date model

set.seed(123) # for reproducibility

# set up the training and test data

Church_attendance_train <- Church_attendance_filter %>% 
  filter(date >= "2022-01-01" & date < "2023-01-01")

Church_attendance_test <- Church_attendance_filter %>% 
  filter(date > max(Church_attendance_train$date))

# set up the first model, it is not great.

model_lm <- lm(attendance ~ date, data = Church_attendance_train) # model does not perform well with just the date

# Let's check the variance and mean of the outcome variable
# given that these are count data to see if a poisson or quasipoisson 
# model would be more appropriate.

# total dataset

(var <- var(Church_attendance$attendance))
(mean <- mean(Church_attendance$attendance))

# filtered dataset for modeling

(var <- var(Church_attendance_filter$attendance))
(mean <- mean(Church_attendance_filter$attendance))

#_______________________________________________________________________________
# Total dataset:
# Looking at the outcome variable (attendance):
# Variance of 3501.036 and mean of 148.1558 
# try a linear regression model and quasipoisson, compare performance.
# Given that we are dealing with count data the variance is far greater than the 
# mean of the data, a quasipoisson model is applicable.
# filtered dataset:
# variance of 895.1764
# mean of 142.306
# quasipoisson still seems to apply or a linear model given that the counts
# are far greater than 0.
# let's use the filtered dataset due to extreme values and problems due to 
# COVID-19 in 2019 - 2020.
#_______________________________________________________________________________

model_lm_add <- lm(attendance ~ date + online + congregation, data = Church_attendance_train)

model_glm_add <- glm(attendance ~ date + online + congregation, data = Church_attendance_train, family = "quasipoisson")

# function for calculating RMSE

rmse <- function(pred.outcome, true.outcome) {
  res <- pred.outcome - true.outcome
  sqrt(mean(res^2))
}

# test the model - calculate pseudo R squared

glance(model_glm_add) %>% summarize(pseudoR2 = 1 - deviance/null.deviance) # pretty good, pseudoR2 = .947
glance(model_lm_add) # performs alright! r.squared = .998

# continue with the linear regression model
# visualize the model

Church_attendance_predictions_lm_sep <- Church_attendance_test %>% 
  mutate(predicted = predict(model_lm_add, newdata = Church_attendance_test)
  )

# make predictions using the quasipoisson model

Church_attendance_predictions_glm_sep <- Church_attendance_test %>% 
  mutate(predicted = predict(model_glm_add, newdata = Church_attendance_test, type = "response")
  )

# check RMSE vs SD for linear model

with(Church_attendance_predictions_lm_sep, rmse(predicted, attendance)) # 39.7577
with(Church_attendance, sd(attendance)) # 59.16956

# check RMSE vs SD for quasipoisson model

with(Church_attendance_predictions_glm_sep, rmse(predicted, attendance)) # 34.38914, better!
with(Church_attendance, sd(attendance)) # 59.16956

# RMSE is quite a bit lower than the SD, so it seems based on r^2 and RMSE vs SD that he quasipoisson could
# help us reduce the errors. The errors are still large given with the lm at 39 people and 
# the quasipoisson at 34.  These are large errors given the raw outcome mean of 148.1558.
# let's give this a shot and see how we do.

# linear table

Church_attendance_predictions_lm <- Church_attendance_test %>% 
  mutate(predicted = predict(model_lm_add, newdata = Church_attendance_test)
  ) %>%
  pivot_longer(cols = c(attendance, predicted),
               names_to = "type",
               values_to = "value"
  )

# quasipoisson table

Church_attendance_predictions_glm <- Church_attendance_test %>% 
  mutate(predicted = predict(model_glm_add, newdata = Church_attendance_test, type = "response")
  ) %>%
  pivot_longer(cols = c(attendance, predicted),
               names_to = "type",
               values_to = "value"
  )

###_____________________________________________________________________________
### Linear plots and analysis
###_____________________________________________________________________________

# Let's take a look at the values together with the predictions.

ggplot(Church_attendance_predictions_lm_sep, aes(x = predicted, y = attendance)) +
  geom_point(alpha = 0.3,
             size = 2,
             color = "red",
             shape = 2
  ) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") # overall the model seems to underestimate church attendance

ggplot(Church_attendance_predictions_lm, aes(date, value, color = type)) +
  geom_point() # further confirmation

# check model performance

GainCurvePlot(Church_attendance_predictions_lm_sep, "predicted", "attendance", "Church Attendance Model - Linear Regression") # good performance

autoplot(model_lm_add, which = 1:4) # errors in this model seem to be a problem

#_______________________________________________________________________________
# Moving forward with the linear model - total attendance predicted by time
#_______________________________________________________________________________

Church_attendance_predictions_lm_sep %>% 
  ggplot(aes(x = date,
             y = predicted
  )
  ) + 
  geom_point(alpha = 0.5,
             size = 2,
             color = "orange"
  ) +
  geom_point(aes(y = attendance),
             color = "darkblue",
             alpha = 0.5
  ) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "darkorange",
              linewidth = 1.5,
              alpha = 0.1
  ) +
  geom_smooth(aes(y = attendance),
              method = "lm",
              se = FALSE,
              color = "darkblue",
              linewidth = 1.5,
              alpha = 0.1 
  ) +
  geom_line(aes(y = cumavg),
            linetype = "dashed",
            linewidth = 2,
            alpha = 0.3,
            color = "darkgreen"
  ) +
  geom_text(aes(x = as.Date("2023-03-15"), y = 160, label = "truth"),
            color = "darkblue",
            size = 4,
            fontface = "bold"
  ) +
  geom_text(aes(x = as.Date("2023-03-16"), y = 121, label = "forecast"),
            color = "darkorange",
            size = 4,
            fontface = "bold"
  ) + 
  geom_text(aes(x = as.Date("2023-04-20"), y = 145, label = "true average"),
            color = "darkgreen",
            size = 4,
            fontface = "bold"
  ) +
  labs(title = "Fitting A Linear Regression Model to the Attendance Data",
       subtitle = "Jan 2023 - April 2023",
       x = "Time",
       y = "Attendance",
       caption = "Dr. Nicolas Foss, MS"
  ) +
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("Church_fit_lm.png", width = 8, height = 6, units = "in")

###_____________________________________________________________________________
### Quasipoisson plots and analysis
###_____________________________________________________________________________

# Let's take a look at the values together with the predictions.

ggplot(Church_attendance_predictions_glm_sep, aes(x = predicted, y = attendance)) +
  geom_point(alpha = 0.3,
             size = 2,
             color = "red",
             shape = 2
  ) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") # overall the model seems to underestimate church attendance

ggplot(Church_attendance_predictions_glm, aes(date, value, color = type)) +
  geom_point() # further confirmation

# check model performance

GainCurvePlot(Church_attendance_predictions_glm_sep, "predicted", "attendance", "Church Attendance Model - Quasipoisson") # strong performance here, too

autoplot(model_glm_add, which = 1:4) # errors in this model seem to be a problem

#_______________________________________________________________________________
# Moving forward with the quasipoisson model - total attendance predicted by time
#_______________________________________________________________________________

Church_attendance_predictions_glm_sep %>% 
  ggplot(aes(x = date,
             y = predicted
  )
  ) + 
  geom_point(alpha = 0.5,
             size = 2,
             color = "orange"
  ) +
  geom_point(aes(y = attendance),
             color = "darkblue",
             alpha = 0.5
  ) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "darkorange",
              linewidth = 1.5,
              alpha = 0.1
  ) +
  geom_smooth(aes(y = attendance),
              method = "lm",
              se = FALSE,
              color = "darkblue",
              linewidth = 1.5,
              alpha = 0.1 
              ) +
  geom_line(aes(y = cumavg),
            linetype = "dashed",
            linewidth = 2,
            alpha = 0.3,
            color = "darkgreen"
  ) +
  geom_text(aes(x = as.Date("2023-03-15"), y = 160, label = "truth"),
            color = "darkblue",
            size = 4,
            fontface = "bold"
  ) +
  geom_text(aes(x = as.Date("2023-03-15"), y = 126, label = "forecast"),
            color = "darkorange",
            size = 4,
            fontface = "bold"
  ) + 
  geom_text(aes(x = as.Date("2023-04-20"), y = 145, label = "true average"),
            color = "darkgreen",
            size = 4,
            fontface = "bold"
  ) +
  labs(title = "Fitting A Quasipoisson Regression Model to the Attendance Data",
       subtitle = "Jan 2023 - April 2023",
       x = "Time",
       y = "Attendance",
       caption = "Dr. Nicolas Foss, MS"
  ) +
  theme(plot.caption = element_text(hjust = 0)) + 
  theme_gdocs()

ggsave("Church_fit.png", width = 8, height = 6, units = "in")

###_____________________________________________________________________________
### Utilize a cross validation strategy to optimize the model.
### Machine learning
###_____________________________________________________________________________

#____________________________________
# cross validation using lm
#____________________________________

Church_attendance_kway_lm <- Church_attendance_filter %>% 
  add_column(pred_cv = 0) %>% 
  filter(date >= "2022-01-01")

splitPlan <- kWayCrossValidation(nrow(Church_attendance_kway_lm), 3, NULL, NULL) # create a split plan

k <- 3 # number of splits

for(i in 1:k) {
  split <- splitPlan[[i]]
  model1 <- lm(attendance ~ date + online + congregation, data = Church_attendance_kway_lm[split$train, ])
  Church_attendance_kway_lm$pred_cv[split$app] <- predict(model1, newdata = Church_attendance_kway_lm[split$app, ])
}

# Look at coefficients

rmse(Church_attendance_kway_lm$pred_cv, Church_attendance_kway_lm$attendance) # 11.91923
sd(Church_attendance$attendance) # 59.16956
autoplot(model1, which = 1:4) # errors are still a problem here
GainCurvePlot(Church_attendance_kway_lm, "pred_cv", "attendance", "Church Attendance Model") # performs alright, not as good as previous
glance(model1) # good r^2 value - 0.863 - not as strong as previous

# seems to perform less well than previous models.  Less complexity is better?

Church_attendance_kway_lm %>%
  ggplot(aes(x = pred_cv, y = attendance)) + 
  geom_point(color = "blue",
             alpha = 0.2
  ) + 
  geom_abline(color = "steelblue",
              alpha = .3,
              linetype = "dashed",
              linewidth = 1.5
              ) +
  theme_gdocs() # still several areas of overestimation and underestimation.

# check distribution of the attendance and predicted values.

Church_attendance_kway_lm %>% 
  pivot_longer(cols = c(pred_cv, attendance),
               names_to = "type",
               values_to = "value"
  ) %>% 
  ggplot(aes(year, value)) +
  geom_boxplot() +
  facet_wrap(~type) # notice how the median 2023 predicted attendance is significantly lower

# model still seems to underestimate and overestimate attendance at times.
# predictions start to fail significantly in 2023.

Church_attendance_kway_lm %>% 
  ggplot(aes(x = pred_cv, y = attendance)) + 
  geom_point(color = "blue",
             alpha = 0.2
  ) + 
  geom_abline(color = "red",
              linewidth = 1.5,
              alpha = 0.3
  ) + 
  theme_clean()

#____________________________________
# cross validation using quasipoisson
#____________________________________

Church_attendance_kway_glm <- Church_attendance_filter %>% 
  add_column(pred_cv = 0) %>% 
  filter(date >= "2022-01-01")

splitPlan_glm <- kWayCrossValidation(nrow(Church_attendance_kway_glm), 3, NULL, NULL) # create a split plan

k <- 3 # number of splits

for(i in 1:k) {
  split_glm <- splitPlan_glm[[i]]
  model2 <- glm(attendance ~ date + online + congregation, data = Church_attendance_kway_glm[split_glm$train, ], family = "quasipoisson")
  Church_attendance_kway_glm$pred_cv[split_glm$app] <- predict(model2, newdata = Church_attendance_kway_glm[split_glm$app, ], type = "response")
}

# Look at coefficients

rmse(Church_attendance_kway_glm$pred_cv, Church_attendance_kway_glm$attendance) # 13.80215
sd(Church_attendance$attendance) # 59.16956
autoplot(model2, which = 1:4) # errors are still a problem here
GainCurvePlot(Church_attendance_kway_glm, "pred_cv", "attendance", "Church Attendance Model") # performs alright
glance(model2) %>% summarize(pseudoR2 = 1 - deviance/null.deviance) # good pseudo r^2 value - 0.824

# seems to perform better than previous models.

Church_attendance_kway_glm %>%
  ggplot(aes(x = pred_cv, y = attendance)) + 
  geom_point(color = "blue",
             alpha = 0.2
  ) + 
  geom_abline(color = "steelblue",
              alpha = .3,
              linetype = "dashed",
              linewidth = 1.5
  ) +
  theme_gdocs() # still several areas of overestimation and underestimation, slightly better than lm.

# check distribution of the attendance and predicted values.

Church_attendance_kway_glm %>% 
  pivot_longer(cols = c(pred_cv, attendance),
               names_to = "type",
               values_to = "value"
  ) %>% 
  ggplot(aes(year, value)) +
  geom_boxplot() +
  facet_wrap(~type) # performance in 2022 is ok, 2023 is still underestimated

# model still seems to underestimate and overestimate attendance at times.
# predictions still fail in 2023.

Church_attendance_kway_glm %>% 
  ggplot(aes(x = pred_cv, y = attendance)) + 
  geom_point(color = "blue",
             alpha = 0.2
  ) + 
  geom_abline(color = "red",
              linewidth = 1.5,
              alpha = 0.3
  ) + 
  theme_gdocs()

###_____________________________________________________________________________
### Try a random forest model to improve.
###_____________________________________________________________________________

ranger_mod <- ranger(attendance ~ date + online + congregation, Church_attendance_train, 
                     num.trees = 500,
                     respect.unordered.factors = "order"
)

ranger_mod # r^2 value far below the other models 0.6306981 

Church_attendance_ranger <- Church_attendance_test

# get predictions for plotting and visualization

Church_attendance_ranger$predicted <- predict(ranger_mod, Church_attendance_ranger)$predictions

# Attendance ~ Predictions - fit is alright

ggplot(Church_attendance_ranger, aes(predicted, attendance)) +
  geom_point() + 
  geom_abline()

# check fraction items in sorted order

GainCurvePlot(Church_attendance_ranger, "predicted", "attendance", "Random Forest Church Attendance Model")

# The model seems to fit poorly, and performs less well than others.

# Check RMSE vs SD

with(Church_attendance_ranger, rmse(predicted, attendance)) # 44.63824
with(Church_attendance, sd(attendance)) # 59.16956

###_____________________________________________________________________________
### Conclusion: This dataset does not provide us enough detail to add on more
### variables to improve the models that.
### This is needed to assist us in improving predictions.
### However, it is possible from this dataset given its raw outcome variable's
### distribution and nature to estimate future attendance with the data given.
###_____________________________________________________________________________
