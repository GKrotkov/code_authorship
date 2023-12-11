#!/usr/bin/env Rscript

# this file takes well_completion as input and predicts six and twelve month
# boe (barrel of oil equivalent)

require(MASS)
require(randomForest)

data <- read.csv("well_completion_clean.csv", header=T)

data$completion_date <- as.Date(data$completion_date, format="%Y-%m-%d")

six <- data.frame(
  well_id=data$well_id,
  latitude=data$latitude, 
  longitude=data$longitude, 
  country=data$country,
  state_province=data$state_province,
  rs_region=data$rs_region, # significant, interacts with above
  rs_basin=data$rs_basin, # log(nn) + significant .496
  rs_play=data$rs_play, 
  rs_sub_play=data$rs_sub_play, 
  trajectory=data$trajectory, # log(nn) + significant .4799
  vintage=data$vintage, # log(nn) + .468
  completion_date=data$completion_date,
  completion_date_timestamp=data$completion_date_timestamp,
  rs_frac_job_type=data$rs_frac_job_type, 
  rs_fluid_type=data$rs_fluid_type, 
  completion_design=data$completion_design,
  rs_proppant_type=data$rs_proppant_type, 
  first_3_month_prod_boe=data$first_3_month_prod_boe,
  first_6_month_prod_boe=data$first_6_month_prod_boe,
  first_12_month_prod_boe=data$first_12_month_prod_boe
) 

# some shorthand notation for formulas farther down
six$y <- six$first_6_month_prod_boe - six$first_3_month_prod_boe
six$x <- six$first_3_month_prod_boe

# FIXME - the cut off points should be determined automagically
six.train <- six[six$completion_date < "2010-01-01",]
six.val <- six[six$completion_date >= "2010-01-01" & six$completion_date < "2013-04-01",]
six.test <- six[data$completion_date >= "2013-04-01",]

# trim data
six.train.x.q = quantile(six.train$x, prob=0.99)
six.train = six.train[six.train$x < six.train.x.q,]

six.train.y.q = quantile(six.train$y, prob=0.99)
six.train = six.train[six.train$y < six.train.y.q,]

# fit data
six.fit.lm.linear <- lm(y ~ x, data=six.train)
six.fit.rlm.linear <- rlm(y ~ x, data=six.train, maxit=40)
six.fit.lm.power <- lm(log(y) ~ log(x), data=six.train)
six.fit.rlm.power <- rlm(log(y) ~ log(x), data=six.train)
six.fit.rf.linear <- randomForest(y ~ x, data=six.train, ntree=10, maxnodes=150)

# val <- read.csv("well_completion_trim_val.csv", header=T)


# use the training distribution to trim the validation distribution
six.val = six.val[six.val$x < six.train.x.q,]

# illegal
#six.val = six.val[six.val$y < six.y.q,]

six.predict.lm.linear = predict(six.fit.lm.linear, six.val)
six.predict.rlm.linear = predict(six.fit.rlm.linear, six.val)
six.predict.lm.power = predict(six.fit.lm.power, six.val)
six.predict.rlm.power = predict(six.fit.rlm.power, six.val)
six.predict.rf.linear = predict(six.fit.rf.linear, six.val)

six.err.lm.linear = six.predict.lm.linear - six.val$y
six.err.rlm.linear = six.predict.rlm.linear - six.val$y
six.err.lm.power = exp(six.predict.lm.power) - six.val$y
six.err.rlm.power = exp(six.predict.rlm.power) - six.val$y
six.err.rf.linear = six.predict.rf.linear - six.val$y

rmse <- function(error)
{
     sqrt(mean(error^2))
}

rmse(six.err.lm.linear)
# 9697.864
rmse(six.err.rlm.linear)
# 9252.763
rmse(six.err.lm.power)
# 20122.54
rmse(six.err.rlm.power)
# 20122.44
rmse(six.err.rf.linear)
# 11399

mae <- function(error)
{
	mean(abs(error))
}

mae(six.err.lm.linear)
#  3929.502
mae(six.err.rlm.linear)
# 3693.08
mae(six.err.lm.power)
# 10720.88
mae(six.err.rlm.power)
# 10720.81
mae(six.err.rf.linear)
# 4279

smoothScatter(fitted(six.fit.rlm.power), residuals(six.fit.rlm.power))

# study first 12 month prod boe based on six and three month

train$delta_12_6_prod_boe = train$first_12_month_prod_boe - train$first_6_month_prod_boe
train$delta_6_3_prod_boe = train$first_6_month_prod_boe - train$first_3_month_prod_boe

twelve.train <- data.frame(y=data$delta_12_6_prod_boe, x1=data$delta_6_3_prod_boe, x2=train$first_3_month_prod_boe)
twelve.train <- twelve.train[complete.cases(twelve.train), ]

twelve.x1.q = quantile(twelve.train$x1, prob=0.99)
twelve.train = twelve.train[twelve.train$x1 < twelve.x1.q,]
twelve.x2.q = quantile(twelve.train$x2, prob=0.99)
twelve.train = twelve.train[twelve.train$x2 < twelve.x2.q,]
twelve.y.q = quantile(twelve.train$y, prob=0.99)
twelve.train = twelve.train[twelve.train$y < twelve.y.q,]

# studied expoential, quadratic, reciprocal, logarithmic, power models

twelve.fit.lm.linear <- lm(y ~ x1 + x2, data=twelve.train)
twelve.fit.rlm.linear <- rlm(y ~ x1 + x2, data=twelve.train, maxit=40)
twelve.fit.lm.inter <- lm(y ~ x1 * x2, data=twelve.train)
twelve.fit.rlm.inter <- rlm(y ~ x1 * x2, data=twelve.train, maxit=40)
twelve.fit.lm.poly <- lm(y ~ poly(x1,3) * poly(x2,3), data=twelve.train)
twelve.fit.rlm.poly <- lm(y ~ poly(x1,3) * poly(x2,3), data=twelve.train)
twelve.fit.lm.exp <- lm(log(y) ~ x1 + x2, data=twelve.train)
twelve.fit.rlm.exp <- rlm(log(y) ~ x1 + x2, data=twelve.train)
twelve.fit.lm.power <- lm(log(y) ~ log(x1) + log(x2), data=twelve.train)
twelve.fit.rlm.power <- rlm(log(y) ~ log(x1) + log(x2), data=twelve.train)


val$delta_12_6_prod_boe = val$first_12_month_prod_boe - val$first_6_month_prod_boe
val$delta_6_3_prod_boe = val$first_6_month_prod_boe - val$first_3_month_prod_boe

twelve.val <- data.frame(y=val$delta_12_6_prod_boe, x1=val$delta_6_3_prod_boe, x2=val$first_3_month_prod_boe)

# legal?
twelve.val = twelve.val[twelve.val$x1 < twelve.x1.q,]
twelve.val = twelve.val[twelve.val$x2 < twelve.x2.q,]

# illegal
#twelve.val = twelve.val[twelve.val$y < q,]

twelve.predict.lm.linear = predict(twelve.fit.lm.linear, twelve.val)
twelve.predict.rlm.linear = predict(twelve.fit.rlm.linear, twelve.val)
twelve.predict.lm.inter = predict(twelve.fit.lm.inter, twelve.val)
twelve.predict.rlm.inter = predict(twelve.fit.rlm.inter, twelve.val)
twelve.predict.lm.poly = predict(twelve.fit.lm.poly, twelve.val)
twelve.predict.rlm.poly = predict(twelve.fit.rlm.poly, twelve.val)
twelve.predict.lm.exp = predict(twelve.fit.lm.exp, twelve.val)
twelve.predict.rlm.exp = predict(twelve.fit.rlm.exp, twelve.val)
twelve.predict.lm.power = predict(twelve.fit.lm.power, twelve.val)
twelve.predict.rlm.power = predict(twelve.fit.rlm.power, twelve.val)

twelve.err.lm.linear = twelve.predict.lm.linear - twelve.val$y
twelve.err.rlm.linear = twelve.predict.rlm.linear - twelve.val$y
twelve.err.lm.inter = twelve.predict.lm.inter - twelve.val$y
twelve.err.rlm.inter = twelve.predict.rlm.inter - twelve.val$y
twelve.err.lm.poly = twelve.predict.lm.poly - twelve.val$y
twelve.err.rlm.poly = twelve.predict.rlm.poly - twelve.val$y
twelve.err.lm.exp = (twelve.predict.lm.exp) - twelve.val$y
twelve.err.rlm.exp = (twelve.predict.rlm.exp) - twelve.val$y
twelve.err.lm.power = exp(twelve.predict.lm.power) - twelve.val$y
twelve.err.rlm.power = exp(twelve.predict.rlm.power) - twelve.val$y

Form = c("Linear", "", "Interaction", "", "Polynomial (3)", "", "Exponential", "", "Power", "")
Algo = c("lm","rlm","lm","rlm","lm","rlm","lm","rlm","lm","rlm")
RootMeanSquareError = c(
rmse(twelve.err.lm.linear),
rmse(twelve.err.rlm.linear),
rmse(twelve.err.lm.inter),
rmse(twelve.err.rlm.inter),
rmse(twelve.err.lm.poly),
rmse(twelve.err.rlm.poly),
rmse(twelve.err.lm.exp),
rmse(twelve.err.rlm.exp),
rmse(twelve.err.lm.power),
rmse(twelve.err.rlm.power))

MeanAbsoluteError = c(
mae(twelve.err.lm.linear),
mae(twelve.err.rlm.linear),
mae(twelve.err.lm.inter),
mae(twelve.err.rlm.inter),
mae(twelve.err.lm.poly),
mae(twelve.err.rlm.poly),
mae(twelve.err.lm.exp),
mae(twelve.err.rlm.exp),
mae(twelve.err.lm.power),
mae(twelve.err.rlm.power))

df = data.frame(Form, Algo, RootMeanSquareError, MeanAbsoluteError)
df
smoothScatter(fitted(twelve.fit.rlm.power), residuals(twelve.fit.rlm.power))

