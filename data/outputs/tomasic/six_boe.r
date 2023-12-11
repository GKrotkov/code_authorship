#!/usr/bin/env Rscript

# this file takes well_completion as input and predicts six and twelve month
# boe (barrel of oil equivalent)

source("setup.r")

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

print(paste("fit data", Sys.time()))

# FIXME - technically you can't max over the entire dataset, only training set
# start neural network experiment
s <- six[six$completion_date >= "2009-01-01" & six$completion_date < "2010-01-01",]
small <- data.frame(y=s$y, x=s$x)
smallval <- data.frame(y=six.val$y, x=six.val$x)
smallvalx <- data.frame(x=six.val$x)

maxs <- apply(small, 2, max)
mins <- apply(small, 2, min)
scaled <- as.data.frame(scale(small, center = mins, scale = maxs - mins))
f <- as.formula("y ~ x")
nn <- neuralnet(f,data=scaled,hidden=c(10,10),linear.output=T)

maxs <- apply(smallvalx, 2, max)
mins <- apply(smallvalx, 2, min)
scaledvalx <- as.data.frame(x=scale(smallvalx, center = mins, scale = maxs - mins))

pr.nn <- compute(nn,scaledvalx)
pr.nn_ <- pr.nn$net.result*(max(small$y)-min(small$y))+min(small$y)
#test.r <- (smallval$y)*(max(smallval$y)-min(smallval$y))+min(smallval$y)
six.err.nn.linear <- smallval$y - pr.nn_
MSE.nn <- sum((six.err.nn.linear)^2)/nrow(smallvalx)
# end neural network experiment

# fit data
six.fit.lm.linear <- lm(y ~ x, data=six.train)
six.fit.rlm.linear <- rlm(y ~ x, data=six.train, maxit=40)
six.fit.lm.power <- lm(log(y) ~ log(x), data=six.train)
six.fit.rlm.power <- rlm(log(y) ~ log(x), data=six.train)
six.fit.lm.mixed <- lm(y ~ x + rs_basin + trajectory + vintage, data=six.train)
six.fit.rlm.mixed <- rlm(y ~ x + rs_basin + trajectory + vintage, data=six.train)
six.fit.rf.linear <- randomForest(y ~ x, data=six.train, ntree=10, maxnodes=150)
six.fit.rf.mixed <- randomForest(y ~ x + rs_basin + trajectory + vintage, data=six.train, ntree=10, maxnodes=150)

# use the training distribution to trim the validation distribution
six.val = six.val[six.val$x < six.train.x.q,]

# illegal
#six.val = six.val[six.val$y < six.y.q,]

print(paste("validate data", Sys.time()))

six.predict.lm.linear = predict(six.fit.lm.linear, six.val)
six.predict.rlm.linear = predict(six.fit.rlm.linear, six.val)
six.predict.lm.power = predict(six.fit.lm.power, six.val)
six.predict.rlm.power = predict(six.fit.rlm.power, six.val)
six.predict.lm.mixed = predict(six.fit.lm.mixed, six.val)
six.predict.rlm.mixed = predict(six.fit.rlm.mixed, six.val)
six.predict.rf.linear = predict(six.fit.rf.linear, six.val)
six.predict.rf.mixed = predict(six.fit.rf.mixed, six.val)

six.err.lm.linear = six.predict.lm.linear - six.val$y
six.err.rlm.linear = six.predict.rlm.linear - six.val$y
six.err.lm.power = exp(six.predict.lm.power) - six.val$y
six.err.rlm.power = exp(six.predict.rlm.power) - six.val$y
six.err.lm.mixed = six.predict.lm.mixed - six.val$y
six.err.rlm.mixed = six.predict.rlm.mixed - six.val$y
six.err.rf.linear = six.predict.rf.linear - six.val$y
six.err.rf.mixed = six.predict.rf.mixed - six.val$y

Form = c("Linear", "", "", "", "Power", "", "Mixed", "", "")
Algo = c("lm","rlm","rf","nn", "lm","rlm",  "lm","rlm", "rf")

RootMeanSquareError = c(
rmse(six.err.lm.linear),
rmse(six.err.rlm.linear),
rmse(six.err.rf.linear),
rmse(six.err.nn.linear),
rmse(six.err.lm.power),
rmse(six.err.rlm.power),
rmse(six.err.lm.mixed),
rmse(six.err.rlm.mixed),
rmse(six.err.rf.mixed)
)

MeanAbsoluteError = c(
mae(six.err.lm.linear),
mae(six.err.rlm.linear),
mae(six.err.rf.linear),
mae(six.err.nn.linear),
mae(six.err.lm.power),
mae(six.err.rlm.power),
mae(six.err.lm.mixed),
mae(six.err.rlm.mixed),
mae(six.err.rf.mixed)
)

df = data.frame(Form, Algo, RootMeanSquareError, MeanAbsoluteError)
print(df)

smoothScatter(fitted(six.fit.rlm.power), residuals(six.fit.rlm.power))

print(paste("finish", Sys.time()))
