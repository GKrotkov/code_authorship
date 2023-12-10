## ----setup, include = FALSE------------------------------------------------------------------------------
# By default, do not include R source code in the PDF. We do not want to see
# code, only your text and figures.
knitr::opts_chunk$set(echo = FALSE)
options(repos = c(CRAN = "https://cran.rstudio.com"))



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(alr4))



## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
hist(Rateprof$quality, breaks = 20, main = "Distribution of Avg Quality Ratings", xlab = "quality")

barplot(table(Rateprof$gender),
        main = "Count of Instructor Gender",
        xlab = "Instructor Gender",
        ylab = "Count")


## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
barplot(table(Rateprof$pepper),
        main = "Distribution of Attractiveness Ratings",
        xlab = "Physical Attractiveness Ratings",
        ylab = "Count")

hist(Rateprof$easiness, breaks = 20, main = "Distribution of Easiness Ratings", xlab = "easiness")


## ---- fig.width=4, fig.height=3--------------------------------------------------------------------------
barplot(table(Rateprof$discipline),
        main = "Distribution of Disciplines",
        xlab = "Discipline",
        ylab = "Count")


## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
boxplot(quality ~ gender, data = Rateprof,
     xlab = "Gender", ylab = "Avg Quality Rating",
     main = "Gender vs. Avg Quality Rating")

boxplot(quality ~ pepper, data = Rateprof,
     xlab = "Attractiveness", ylab = "Avg Quality Rating",
     main = "Attractiveness vs. Avg Quality Rating")



## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
plot(Rateprof$easiness, Rateprof$quality,
     xlab = "Easiness Rating", ylab = "Avg Quality Rating",
     main = "Easiness Rating vs. Avg Quality Rating")

boxplot(quality ~ discipline, data = Rateprof,
     xlab = "Discipline", ylab = "Avg Quality Rating",
     main = "Discipline vs. Avg Quality Rating")


## --------------------------------------------------------------------------------------------------------
model_1 <- lm(quality ~ gender + pepper + easiness + discipline, data = Rateprof)

model_2 <- lm(quality ~ gender + pepper + easiness + discipline + gender*easiness + discipline*easiness, data = Rateprof)


## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
plot(model_1, which = 2, main = "Q-Q Plot for Model 1")

plot(model_2, which = 2, main = "Q-Q Plot for Model 2")



## ---- fig.width=3.5, fig.height=3------------------------------------------------------------------------
plot(model_1, which = 1, main = "Residuals vs Fitted for Model 1")

plot(model_2, which = 1, main = "Residuals vs Fitted for Model 2")



## ---- warning=FALSE--------------------------------------------------------------------------------------
library(modelsummary)
modelsummary(list("Model 1" = model_1), stars = TRUE)



## --------------------------------------------------------------------------------------------------------
conf_intervals <- data.frame(
  Variable = c("(Intercept)", "gendermale", "pepperyes", "easiness", "disciplineSocSci", "disciplineSTEM", "disciplinePre-prof"),
  `2.5 %` = c(1.261572839, 0.004884619, 0.444077454, 0.476750306, -0.162053608, -0.004926202, -0.221086987),
  `97.5 %` = c(1.8960917, 0.2845138, 0.8665078, 0.6599858, 0.2269213, 0.3468177, 0.1749500)
)

library(knitr)

kable(conf_intervals, format = "markdown", align = "c")



## --------------------------------------------------------------------------------------------------------
anova_table <- data.frame(
  Model = c("Model 1", "Model 2"),
  Res.Df = c(359, 355),
  RSS = c(154.60, 154.05),
  Df = c(NA, 4),
  Sum_of_Sq = c(NA, 0.54359),
  F = c(NA, 0.3132),
  `Pr(>F)` = c(NA, 0.8691)
)

library(knitr)

kable(anova_table, format = "markdown", align = "c")



## --------------------------------------------------------------------------------------------------------
library(modelsummary)
modelsummary(list("Model 1" = model_1, "Model 2" = model_2), stars = TRUE)

