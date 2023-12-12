## ----setup, include = FALSE------------------------------------------------------------------------------
# By default, do not include R source code in the PDF. We do not want to see
# code, only your text and figures.
knitr::opts_chunk$set(echo = FALSE)


## ---- include=FALSE--------------------------------------------------------------------------------------
library(car)
library(carData)
library(effects)
library(alr4)

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(modelsummary)
library(patchwork)


## ---- include=FALSE--------------------------------------------------------------------------------------
head(Rateprof)

#DATA = Rateprof
#quality = response var
#gender, pepper (attractiveness), easiness, discipline = explanatory variables


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Histogram of Average Quality Rating."---------
ggplot(Rateprof, aes(x = quality)) +
  geom_histogram() +
  labs(x = "Average Quality Rating, between 1 (worst) to 5 (best)",
       y = "Count",
       title = "Quality Histogram")


## --------------------------------------------------------------------------------------------------------
gender.eda = ggplot(Rateprof, aes(x = gender)) +
  geom_bar() +
  labs(x = "Gender",
       y = "Count",
       title = "Gender Barchart")

pepper.eda = ggplot(Rateprof, aes(x = pepper)) +
  geom_bar() +
  labs(x = "Attractive (yes or no)",
       y = "Count",
       title = "Atractiveness Barchart")

discipline.eda = ggplot(Rateprof, aes(x = discipline)) +
  geom_bar() +
  labs(x = "Discipline",
       y = "Count",
       title = "Discipline Barchart")


## ---- message = FALSE, fig.width=5, fig.height=4, fig.cap="Barcharts of Categorical Predictors"----------
(gender.eda + pepper.eda)/discipline.eda


## ---- message = FALSE, fig.width=4.5, fig.height=3, fig.cap="Histogram of Average Easiness Rating."------
ggplot(Rateprof, aes(x = easiness)) +
  geom_histogram() +
  labs(x = "Average Easiness Rating, between 1 (worst) to 5 (best)",
       y = "Count",
       title = "Easiness Histogram")


## --------------------------------------------------------------------------------------------------------
g.q = ggplot(Rateprof, aes(y = quality, x = gender)) +
  geom_boxplot() +
  labs(y = " ",
       x = "Gender",
       title = "Quality vs. Gender")

a.q = ggplot(Rateprof, aes(y = quality, x = pepper)) +
  geom_boxplot() +
  labs(y = " ",
       x = "Attractiveness",
       title = "Quality vs. Attractiveness")

d.q = ggplot(Rateprof, aes(y = quality, x = discipline)) +
  geom_boxplot() +
  labs(y = " ",
       x = "Discipline",
       title = "Quality vs. Discipline")


## ---- message = FALSE, fig.width=4.8, fig.height=3.75, fig.cap="Boxplots of Quality vs. Categorical Predictors. Y axis constant for all: Average Quality Rating, between 1 (worst) to 5 (best)"----
(g.q + a.q)/d.q


## ---- message = FALSE, fig.width=4.1, fig.height=4, fig.cap="Scatterplot of Quality vs. Easiness."-------
ggplot(Rateprof, aes(y = quality, x = easiness)) +
  geom_point() +
  labs(y = "Average Quality Rating, between 1 (worst) to 5 (best)",
       x = "Average Easiness Rating, between 1 (worst) to 5 (best)",
       title = "Quality vs. Easiness")


## --------------------------------------------------------------------------------------------------------
in2.model = lm(quality~gender + pepper + easiness + discipline + 
                 easiness*gender + easiness*pepper + easiness*discipline, data = Rateprof)
#summary(in2.model)


## ---- results='hide', include = FALSE--------------------------------------------------------------------
#cook's distance
cookd.in2 = cooks.distance(in2.model)
cookd.in2
which.max(cookd.in2)

head(augment(in2.model))

in2.plot = augment(in2.model) %>%
  ggplot(aes(x = easiness, y = .cooksd)) +
  geom_point() + 
  labs(y = "Cook's Distance",
       x = "Average Easiness Rating, between 1 (worst) to 5 (best)",
       title = "Cook's Distance vs. Easiness")
#seems fine

sort(pf(cookd.in2,12,354), decreasing = TRUE) #none of the points have cooks distances above the 50th percentile of f distribution with df q=11 and n-q so none are influential enough to remove 
curve(df(x, df1=12, df2=354))


## ---- message = FALSE, fig.width=4.5, fig.height=3, fig.cap="Plot of the Cook's Distance for the Model."----
in2.plot


## --------------------------------------------------------------------------------------------------------
in2.plotr.e = augment(in2.model) %>%
  ggplot(aes(x = `easiness`, y = .resid)) +
  geom_point() +
  labs(y = "Residuals",
       x = "Easiness Rating")

in2.plotr.g = augment(in2.model) %>%
  ggplot(aes(x = `gender`, y = .resid)) +
  geom_boxplot() +
  labs(y = "Residuals",
       x = "Gender")

in2.plotr.p = augment(in2.model) %>%
  ggplot(aes(x = `pepper`, y = .resid)) +
  geom_boxplot() +
  labs(y = "Residuals",
       x = "Atractive (yes/no)")

in2.plotr.d = augment(in2.model) %>%
  ggplot(aes(x = `discipline`, y = .resid)) +
  geom_boxplot() +
  labs(y = "Residuals",
       x = "Discipline")


## --------------------------------------------------------------------------------------------------------
residualplots = ggarrange(in2.plotr.e, in2.plotr.g, in2.plotr.d, in2.plotr.p, ncol = 2, nrow = 2)


## ---- message = FALSE, fig.width=4.5, fig.height=3, fig.cap="Residuals Plots for all Predictor Variables of the Model."----
residualplots


## --------------------------------------------------------------------------------------------------------
in2.plotqq =
  ggplot(augment(in2.model), aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()+
  labs(y = "Standardized Residuals",
       x = "Theoretical Quantiles",
       title = "Normal Q-Q Plot")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="QQ Plot for the Model."-----------------------
in2.plotqq


## --------------------------------------------------------------------------------------------------------
plain.model = lm(quality~gender + pepper + easiness + discipline, data = Rateprof)


## ---- results = 'hide'-----------------------------------------------------------------------------------
car::vif(plain.model)


## ---- results='hide'-------------------------------------------------------------------------------------
anova(in2.model, plain.model)


## --------------------------------------------------------------------------------------------------------
modelsummary(list("Full Model" = in2.model, "Reduced Model" = plain.model), 
             statistic = c("conf.int", "p.value"),
             gof_map = c("r.squared","nobs", "aic"))

