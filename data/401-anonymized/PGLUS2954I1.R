## ----setup, include = FALSE------------------------------------------------------------------------------
# By default, do not include R source code in the PDF. We do not want to see
# code, only your text and figures.
knitr::opts_chunk$set(echo = FALSE)


## ---- message=FALSE--------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(broom)


## --------------------------------------------------------------------------------------------------------
sleep_data = read.csv("/Users/Jane/Desktop/CMU/Year 4/401/Data Exam 1/cmu-sleep.csv")
sleep_data = sleep_data %>%
  mutate(TotalSleepTime = TotalSleepTime/60)


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Histogram of time students spent sleeping at night."----
ggplot(sleep_data, aes(x = TotalSleepTime)) +
  geom_histogram() +
  labs(x = "Time spent sleeping at night (hours)",
       y = "Count",
       title = "Sleep Time Histogram")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Histogram of student's GPA the semester the study occured."----
ggplot(sleep_data, aes(x = term_gpa)) +
  geom_histogram() +
  labs(x = "Student's term GPA (out of 4)",
       y = "Count",
       title = "Term GPA Histogram")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Histogram of student's GPA prior to the study occuring."----
ggplot(sleep_data, aes(x = cum_gpa)) +
  geom_histogram() +
  labs(x = "Student's cumulative GPA (out of 4)",
       y = "Count",
       title = "Cumulative GPA Histogram")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Scatterplot of term GPA vs. sleep time."------
ggplot(sleep_data, aes(y = term_gpa, x = TotalSleepTime)) +
  geom_point() +
  labs(y = "Student's term GPA (out of 4)",
       x = "Nightly sleep time (hours)",
       title = "Term GPA vs. Sleep Time")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Scatterplot of cumulative GPA vs. sleep time."----
ggplot(sleep_data, aes(y = cum_gpa, x = TotalSleepTime)) +
  geom_point() +
  labs(y = "Student's cumulative GPA (out of 4)",
       x = "Nightly sleep time (hours)",
       title = "Cumulative GPA vs. Sleep Time")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Scatterplot of term GPA vs. cumulative GPA."----
ggplot(sleep_data, aes(y = term_gpa, x = cum_gpa)) +
  geom_point() +
  labs(y = "Student's term GPA (out of 4)",
       x = "Student's cumulative GPA (out of 4)",
       title = "Term GPA vs. Cumulative GPA")


## ---- results='hide'-------------------------------------------------------------------------------------
#fitting model
term.model = lm(term_gpa~TotalSleepTime, data = sleep_data)


## ---- results='hide', include = FALSE--------------------------------------------------------------------
#cook's distance
cookd.term = cooks.distance(term.model)
which.max(cookd.term)

head(augment(term.model))

plot = augment(term.model) %>%
  ggplot(aes(x = TotalSleepTime, y = .cooksd)) +
  geom_point() + 
  labs(y = "Cook's Distance",
       x = "Total Nightly Sleep Time (hours)",
       title = "Cook's Distance vs. Sleep Time")

sort(pf(cookd.term,2,632), decreasing = TRUE) #none of the points have cooks distances above the 50th percentile of f distribution with df 2 and n-2 so none are influential enough to remove 
curve(df(x, df1=2, df2=632))


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Plot of the Cook's Distance for Term GPA Model."----
plot


## --------------------------------------------------------------------------------------------------------
#plot of residuals v sleep time
plotr = augment(term.model) %>%
  ggplot(aes(x = `TotalSleepTime`, y = .resid)) +
  geom_point() +
  labs(y = "Residuals",
       x = "Total Nightly Sleep Time (hours)",
       title = "Residuals vs. Sleep Time")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="Plot of the Residuals for Term GPA Model."----
plotr


## --------------------------------------------------------------------------------------------------------
#qq plot
plotqq =
  ggplot(augment(term.model), aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(y = "Standardized Residuals",
       x = "Theoretical Quantiles",
       title = "Normal Q-Q Plot")


## ---- message = FALSE, fig.width=4, fig.height=3, fig.cap="QQ Plot for Term GPA Model."------------------
plotqq


## ---- results='hide'-------------------------------------------------------------------------------------
summary(term.model)


## ---- results='hide'-------------------------------------------------------------------------------------
confint(term.model, "TotalSleepTime", 0.95)
2*confint(term.model, "TotalSleepTime", 0.95)

