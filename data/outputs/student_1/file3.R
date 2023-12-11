## ----setup, include = FALSE------------------------------------------------------------------------------
# By default, do not include R source code in the PDF. We do not want to see
# code, only your text and figures.
knitr::opts_chunk$set(echo = FALSE)


## ---- include = FALSE------------------------------------------------------------------------------------
library(alr4)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(data.table)
library(broom)
Rateprof = Rateprof


## ---- fig.width=5, fig.height=4, fig.cap="The distribution of instructor quality ratings is slightly left skewed with a mean of 3.575. Most professors receive mid-high ratings, however a few recieved very low ratings."----
hist(Rateprof$quality, xlab = "Avg Quality Rating from 1-5", main = "")


## ---- fig.width=5, fig.height=3, fig.cap="The distribution of instructor easiness ratings is symmetric and approximately normal, with a mean of 3.13 and a standard deviation of 0.778."----
hist(Rateprof$easiness, xlab = "Avg Easiness Rating from 1-5", main = "")


## ---- fig.width=5, fig.height=3, fig.cap="There are more male professors than female professors, however the counts are relatively even."----
barplot(table(Rateprof$gender), names = c("Female", "Male"), xlab = "Gender", ylab = "Count",  main = "")


## ---- fig.width=3, fig.height=3, fig.cap="The vast majority of professors are rated as not attractive."----
barplot(table(Rateprof$pepper), names = c("No", "Yes"), xlab = "Attractive?", ylab = "Count",  main = "")


## ---- fig.width=5, fig.height=4, fig.cap="The most popular discipline among professors is humanities, with STEM coming in second. Social Sciences and Pre-Professional are the least popular disciplines, with the latter having slightly less professors."----
barplot(table(Rateprof$discipline), names = c("Hum", "SocSci", "STEM", "Pre-prof"), xlab = "Discipline", ylab = "Count",  main = "")


## ---- fig.width=4, fig.height=3.5, fig.cap="The scatterplot of quality vs easiness ratings reveals that the two variables have a positive, linear relationship."----
plot(quality~easiness, data = Rateprof)


## ---- fig.width=5, fig.height=3, fig.cap="Both genders have a similar median rating and spread of ratings."----
boxplot(quality~gender, data = Rateprof)


## ---- fig.width=5, fig.height=3, fig.cap="Attractive professors have a higher median rating around 4.5 and the range of ratings is smaller, while less attractive professors have a lower median rating around 3.5 and the range of ratings is much larger."----
boxplot(quality~pepper, data = Rateprof)


## ---- fig.width=5, fig.height=3, fig.cap="Ratings across all 4 disciplines tend to be pretty similar, with social sciences having a slightly higher median rating and STEM professors having a slightly lower median rating."----
plot(quality~discipline, data = Rateprof)


## ---- include=FALSE--------------------------------------------------------------------------------------
lm1 <- lm(quality ~ gender + pepper + easiness + discipline, data = Rateprof)
out1 <- summary(lm1)


## ---- fig.width=5, fig.height=3.6, fig.cap = "Relatively even spread and lack of pattern in the residual plot for our first linear model indicate assumptions are not violated."----
res <- resid(lm1)
plot(fitted(lm1), res,
main = "", xlab = "fitted values", ylab = "residuals")
abline(0,0, col = "red")


## ---- fig.width=5, fig.height=3.5, fig.cap= "A QQ plot of our linear model's errors reveals a heavy left tail and a light right tail, indicating left skewness and non-normality."----
qqnorm(residuals(lm1), main = "")
qqline(residuals(lm1))


## ---- include= FALSE-------------------------------------------------------------------------------------
lmfull <- lm(quality ~ easiness + gender + discipline + easiness:gender + easiness:discipline, data = Rateprof)
outfull<- summary(lmfull)

lmreduced <- lm(quality ~ easiness + gender + discipline, data = Rateprof)
outreduced<-summary(lmreduced)
f_test <- anova(lmreduced, lmfull)


## ---- fig.width=5, fig.height=3.5, fig.cap= "A QQ plot of the full model's errors reveals a heavy left tail and a light right tail, indicating left skewness and some non-normality."----
qqnorm(residuals(lmfull), main = "")
qqline(residuals(lmfull))


## ---- include=FALSE--------------------------------------------------------------------------------------
var_names=names(coef(lm1))
coef_vals=coef(lm1)


## ---- fig.width=5, fig.height=3, fig.cap="Table of regression coefficients for linear model."------------
data.table(Variables=var_names, `Regression Coefficients`=coef_vals)


## ---- include=FALSE--------------------------------------------------------------------------------------
var_names2=names(coef(lmfull))
coef_valsfull=coef(lmfull)
coef_valsreduced = c(coef(lmreduced), "NA", "NA", "NA", "NA")


## ---- fig.width=5, fig.height=3, fig.cap="Table of regression coefficients for linear model."------------
data.table(Variables=var_names2, `Coefs Full`=coef_valsfull, `Coefs Reduced` = coef_valsreduced)


## --------------------------------------------------------------------------------------------------------
f_test

