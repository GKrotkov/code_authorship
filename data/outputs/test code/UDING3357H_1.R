## --------------------------------------------------------------------------------------------------------
suppressMessages(library(dplyr))

# Loading the dataset
cmu_sleep <- read.csv("cmu-sleep.csv")


## ---- fig.width=4, fig.height=3, fig.cap="Histogram of Total Sleep Time"---------------------------------
hist(cmu_sleep$TotalSleepTime, breaks = 20, main = "Distribution of Total Sleep Time", xlab = "Total Sleep Time (minutes)")


## ---- fig.width=4, fig.height=3, fig.cap="Histogram of Term GPA"-----------------------------------------
hist(cmu_sleep$term_gpa, breaks = 20, main = "Distribution of Term GPA", xlab = "Term GPA")


## ---- fig.width=4, fig.height=3, fig.cap="Histogram of Cumulative GPA"-----------------------------------
hist(cmu_sleep$cum_gpa, breaks = 20, main = "Distribution of Cumulative GPA", xlab = "Cumulative GPA")


## ---- fig.width=5, fig.height=3, fig.cap="Scatterplot of Total Sleep Time vs. Term GPA"------------------
plot(cmu_sleep$TotalSleepTime, cmu_sleep$term_gpa, 
     xlab = "Total Sleep Time (minutes)", ylab = "Term GPA",
     main = "Total Sleep Time vs. Term GPA")


## ---- fig.width=4, fig.height=3, fig.cap="Normal Q-Q Plot of Resdiuals"----------------------------------
lm_model <- lm(term_gpa ~ TotalSleepTime, data = cmu_sleep)

qqnorm(resid(lm_model))
qqline(resid(lm_model))


## ---- fig.width=5, fig.height=3, fig.cap="Residuals vs. Fitted Values"-----------------------------------
plot(lm_model, which = 1)


## --------------------------------------------------------------------------------------------------------
lm_model1 <- lm(term_gpa ~ TotalSleepTime, data = cmu_sleep)

summary(lm_model1)


## --------------------------------------------------------------------------------------------------------
# Load the "car" package for confidence intervals
suppressMessages(library(car))
confint(lm_model1)


## --------------------------------------------------------------------------------------------------------
adjusted_sleep_time_data <- data.frame(TotalSleepTime = mean(cmu_sleep$TotalSleepTime) - 120)
predicted_gpa <- predict(lm_model, newdata = adjusted_sleep_time_data, interval = "prediction", level = 0.95)
predicted_gpa



## --------------------------------------------------------------------------------------------------------
lm_model3 <- lm(term_gpa ~ TotalSleepTime + cum_gpa, data = cmu_sleep)

summary(lm_model3)


## --------------------------------------------------------------------------------------------------------
confint(lm_model3, "cum_gpa")

