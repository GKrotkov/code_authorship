## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND PLOT HERE


## --------------------------------------------------------------------------------------------------------
library(tidyverse)
heart <- read.csv("https://raw.githubusercontent.com/FSKoerner/Fall23-36315-data/main/heart.csv")
#ensure the categorical variables are factors
heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$slope <- factor(heart$slope)
heart$thal <- factor(heart$thal)
heart$target <- factor(heart$target)


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=age, y=oldpeak), data = heart) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatterplot of ST depression and age with linear regression line", 
       x = "age", y = "ST depression")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=age, y=oldpeak), data = heart) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "pink") + 
  labs(title = "Scatterplot of ST depression and age with quadratic regression line", 
       x = "age", y = "ST depression")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=age, y=oldpeak), data = heart) + geom_point() + 
  geom_smooth(method = "loess", se = FALSE, color = "red") + 
  labs(title = "Scatterplot of ST depression and age with local regression line", 
       x = "age", y = "ST depression")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=age, y=oldpeak), data = heart) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "pink") +
  geom_smooth(method = "loess", se = FALSE, color = "red") + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of ST depression and age with regression lines", 
       x = "age", y = "ST depression")


## --------------------------------------------------------------------------------------------------------
linear_model <- lm(oldpeak~age, data = heart)
outlm<- summary(linear_model)
outlm$coefficients
confint(linear_model)


## --------------------------------------------------------------------------------------------------------

ggplot(linear_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) + labs(title = "Residual Plot") + 
  geom_smooth(method="loess")


## --------------------------------------------------------------------------------------------------------
ggplot(heart, aes(sample=oldpeak)) +
  stat_qq() + 
  stat_qq_line()


## --------------------------------------------------------------------------------------------------------
oldPeakNotZero <- heart %>% filter(oldpeak != 0)


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=oldpeak), data = oldPeakNotZero) + geom_bar(fill="pink", color="red") + 
  labs(title = "Distribution of oldpeak within the dataset where oldpeak!=0", 
       x = "oldpeak value", y = "frequency")
ggplot(aes(x=sqrt(oldpeak)), data = oldPeakNotZero) + geom_bar(fill="pink", color="red") + 
  labs(title = "Distribution of sqrt(oldpeak) within the dataset where oldpeak!=0", 
       x = "oldpeak value", y = "frequency")


## --------------------------------------------------------------------------------------------------------
ggplot(aes(x=thalach, y=sqrt(oldpeak)), data=oldPeakNotZero) + geom_point()+
  geom_smooth(method = "lm", se=FALSE) + 
  labs(title = "Scatterplot of sqrt(oldpeak) and thalach, with regression line", 
       x = "thalach", y = "sqrt(oldpeak)")
linear_model2 <- lm(sqrt(oldpeak)~thalach, data = oldPeakNotZero)
coef(linear_model2)[2]


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------
library(tidyverse)
#plot1 code
plot1 <- ggplot(heart,
	aes(x = chol, y = thalach)) +
geom_point(aes(color = target, shape = sex)) +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 1")
#plot2 code
plot2 <- ggplot(heart,
	aes(x = chol, y = thalach, color = target)) +
geom_point(aes(shape = sex)) +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 2")
#plot3 code
plot3 <- ggplot(heart,
	aes(x = chol, y = thalach, shape = sex)) +
geom_point(aes(color = target)) +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 3")
#plot4 code
plot4 <- ggplot(heart,
	aes(x = chol, y = thalach, shape = sex, color = target)) +
geom_point() +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 4")

library(gridExtra)
#arrange the plot
grid.arrange(plot1, plot2, plot3, plot4)


## --------------------------------------------------------------------------------------------------------
plot3copy <- ggplot(heart,
	aes(x = chol, y = thalach, color = sex)) +
geom_point(aes(shape = target)) +
geom_smooth(method = lm, se = FALSE, aes(color = sex)) + labs(title = "Plot 3")
plot3copy


## --------------------------------------------------------------------------------------------------------
target1Data <- heart %>% filter(target == 1)
target0Data <- heart %>% filter(target == 0)
maleData <- heart %>% filter(sex == 1)
femaleData <- heart %>% filter(sex == 0)


## --------------------------------------------------------------------------------------------------------
ggplot(target1Data,
	aes(x = chol, y = thalach, color = sex)) +
geom_point() +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 4 subsetted by Target=1")

ggplot(target0Data,
	aes(x = chol, y = thalach, color = sex)) +
geom_point() +
geom_smooth(method = lm, se = FALSE) + labs(title = "Plot 4 subsetted by Target=0")


## --------------------------------------------------------------------------------------------------------
summary(lm(thalach ~ chol*sex, data = heart))


## --------------------------------------------------------------------------------------------------------
# PUT ANY NECESSARY CODE HERE

