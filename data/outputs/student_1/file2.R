## ----setup, include = FALSE------------------------------------------------------------------------------
# By default, do not include R source code in the PDF. We do not want to see
# code, only your text and figures.
knitr::opts_chunk$set(echo = FALSE)


## ---- include = FALSE------------------------------------------------------------------------------------
library(tidyverse)
sleep <- read_csv("cmu-sleep.csv")


## ---- fig.height = 3.5, fig.cap= "Total Sleep Time is bell-shaped and relatively normally distributed. It has a peak somewhere between 400 and 475 minutes."----
#Histogram of Total Sleep Time
hist(sleep$TotalSleepTime, main = "Total Sleep Time Per Night", xlab = "Time (in minutes)", ylab = "Number of Students")


## ---- fig.height = 3.5, fig.cap= "Term GPA has a large left skew and no left tail as the max is 4.0. Most students achieved a GPA of between 3.0 and 4.0 this past term."----
#Histogram of Term GPA
hist(sleep$term_gpa, main = "Term GPA", xlab = "GPA (out of 4.0)", ylab = "Number of Students", breaks = 20)


## ---- fig.height = 3.5, fig.cap= "Cumulative GPA is also left skewed. The peak is at a GPA of around 3.75, with the left tail being very thin towards the GPAs of 1.5-2.5, indicating that most students received a GPA of between 3.0 and 4.0."----
#Histogram of Cumulative GPA
hist(sleep$cum_gpa, main = "Cumulative GPA ", xlab = "GPA (out of 4.0)", ylab = "Number of Students")


## ---- fig.height = 3.5, fig.cap= "The scatter plot of Term GPA and Total Sleep Time is clustered towards the top and middle, indicating that most students had a GPA of between 3.0 and 4.0, and the sleep times of students varies rather largely. It is difficult to see any kind of relationship as the higher GPA values are clustered so closely together."----
#Scatterplot of Total Sleep Time and Term GPA
plot(term_gpa ~ TotalSleepTime, data = sleep, main = "Term GPA vs Sleep Time", ylab = "GPA (out of 4.0)", xlab = "Sleep per Night (in minutes)")


## ---- fig.height = 3.5, fig.cap= "The same is true to to an extent for the plot of Cumulative GPA and Total Sleep Time, although it does look slightly more spread out so a flat line may be able to be drawn, indicating that there is no/a small positive linear relationship."----
#Scatterplot of Total Sleep Time and Cumulative GPA
plot(cum_gpa ~ TotalSleepTime, data = sleep, main = "Cum GPA vs Sleep Time", ylab = "GPA (out of 4.0)", xlab = "Sleep per Night (in minutes)")


## ---- fig.height = 3.5, fig.cap= "The scatter plot of term GPA and cumulative GPA shows a strong positive linear relationship between the two variables, a fact that will be useful in answering our third research question."----
#Scatterplot of GPAs
plot(term_gpa ~ cum_gpa, data = sleep, main = "Term GPA vs Cum GPA", ylab = "GPA (out of 4.0)", xlab = "GPA (out of 4.0)")


## ---- fig.height = 3.5, fig.cap= "A log transformation of sleep time did not seem to improve the spread of the scatter plot with respect to term GPA, so a model will not be fitted with respect to this transformation."----
#Scatterplot of Total Sleep Time and Log of Term GPA
plot(term_gpa ~ log(TotalSleepTime), data = sleep, main = "Term GPA vs log(Sleep)", ylab = "GPA (out of 4.0)", xlab = "Log of Sleep per Night (in minutes)")


## ---- include=FALSE--------------------------------------------------------------------------------------
#Linear model of Total Sleep Time and Term GPA
term_vs_sleep <- lm(term_gpa ~ TotalSleepTime, data = sleep)

#Linear model of Total Sleep Time and Cumulative GPA
cum_vs_sleep <- lm(cum_gpa ~ TotalSleepTime, data = sleep)

#summary of first linear model
out1 <- summary(term_vs_sleep)
out1

#summary of second linear model
out2 <- summary(cum_vs_sleep)
out2

summary(lm(term_gpa ~ TotalSleepTime + cum_gpa, data = sleep))


## ---- fig.height3.7, fig.cap = "The residual plot of term GPA suffers from heteroskedasticity, indicating non-constant variance of the errors"----
plot(sleep$TotalSleepTime, out1$residuals,
main = "Plot of term residuals vs total sleep time", ylim = c(-2,2), xlab = "total sleep time", ylab = "residuals")
abline(h=0, v = 0, col = "red")


## ---- fig.height=3.7, fig.cap = "The residual plot of cumulative GPA suffers from heteroskedasticity, indicating non-constant variance of the errors"----
plot(sleep$TotalSleepTime, out2$residuals,
main = "Plot of cumulative residuals vs total sleep time", ylim = c(-2,2), xlab = "total sleep time", ylab = "residuals")
abline(h=0, v = 0, col = "red")


## ---- fig.height=3.8, fig.cap = "The normal Q-Q plot of term GPA has deviation from the normal line."----
qqnorm(residuals(term_vs_sleep), main = "Normal Q-Q Plot (Term GPA)", ylim = c(-4,4))
qqline(residuals(term_vs_sleep))


## ---- fig.height=3.8, fig.cap = "The normal Q-Q plot of cumulative GPA has deviation from the normal line."----
qqnorm(residuals(cum_vs_sleep), main = "Normal Q-Q Plot (Cumulative GPA)", ylim = c(-4,4))
qqline(residuals(cum_vs_sleep))


## ---- include=FALSE--------------------------------------------------------------------------------------
CI_term <- confint(term_vs_sleep)
CI_cum <- confint(cum_vs_sleep)
CI_term
CI_cum


## ---- include=FALSE--------------------------------------------------------------------------------------
lm3 <- lm(term_gpa ~ TotalSleepTime + cum_gpa, data = sleep)
out3 <- summary(lm3)
out3

