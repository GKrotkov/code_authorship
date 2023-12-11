
source("setup.r")

# study first 12 month prod boe based on six and three month

data$delta_12_6_prod_boe = data$first_12_month_prod_boe - data$first_6_month_prod_boe
data$delta_6_3_prod_boe = data$first_6_month_prod_boe - data$first_3_month_prod_boe

twelve <- data.frame(
#  well_id=data$well_id,
#  latitude=data$latitude, 
#  longitude=data$longitude, 
#  country=data$country,
#  state_province=data$state_province,
#  rs_region=data$rs_region, # significant, interacts with above
  rs_basin=data$rs_basin, # log(nn) + significant .496
#  rs_play=data$rs_play, 
#  rs_sub_play=data$rs_sub_play, 
  trajectory=data$trajectory, # log(nn) + significant .4799
  vintage=data$vintage, # log(nn) + .468
  completion_date=data$completion_date,
#  completion_date_timestamp=data$completion_date_timestamp,
#  rs_frac_job_type=data$rs_frac_job_type, 
#  rs_fluid_type=data$rs_fluid_type, 
#  completion_design=data$completion_design,
#  rs_proppant_type=data$rs_proppant_type, 
  x2=data$first_3_month_prod_boe,
  first_6_month_prod_boe=data$first_6_month_prod_boe,
  first_12_month_prod_boe=data$first_12_month_prod_boe,
  x1=data$delta_6_3_prod_boe,
  y=data$delta_12_6_prod_boe
) 

twelve <- twelve[complete.cases(twelve), ]

# FIXME - the cut off points should be determined automagically
twelve.train <- twelve[twelve$completion_date < "2010-01-01",]
twelve.val <- twelve[twelve$completion_date >= "2010-01-01" & twelve$completion_date < "2013-04-01",]
twelve.test <- twelve[data$completion_date >= "2013-04-01",]

twelve.x1.q = quantile(twelve.train$x1, prob=0.99)
twelve.train = twelve.train[twelve.train$x1 < twelve.x1.q,]
twelve.x2.q = quantile(twelve.train$x2, prob=0.99)
twelve.train = twelve.train[twelve.train$x2 < twelve.x2.q,]
twelve.y.q = quantile(twelve.train$y, prob=0.99)
twelve.train = twelve.train[twelve.train$y < twelve.y.q,]

# studied expoential, quadratic, reciprocal, logarithmic, power models

print(paste("fit data", Sys.time()))


twelve.fit.lm.linear <- lm(y ~ x1 + x2, data=twelve.train)
twelve.fit.rlm.linear <- rlm(y ~ x1 + x2, data=twelve.train, maxit=40)
twelve.fit.rf.linear <- randomForest(y ~ x1 + x2, data=twelve.train, ntree=10, maxnodes=150)
twelve.fit.lm.mixed <- lm(y ~ x1 + x2 + rs_basin + trajectory + vintage, data=twelve.train)
twelve.fit.rlm.mixed <- rlm(y ~ x1 + x2 + rs_basin + trajectory + vintage, data=twelve.train, maxit=40)
twelve.fit.rf.mixed <- randomForest(y ~ x1 + x2 + rs_basin + trajectory + vintage, data=twelve.train, ntree=10, maxnodes=150)
twelve.fit.lm.inter <- lm(y ~ x1 * x2, data=twelve.train)
twelve.fit.rlm.inter <- rlm(y ~ x1 * x2, data=twelve.train, maxit=40)
twelve.fit.lm.poly <- lm(y ~ poly(x1,3) * poly(x2,3), data=twelve.train)
twelve.fit.rlm.poly <- lm(y ~ poly(x1,3) * poly(x2,3), data=twelve.train)
twelve.fit.lm.exp <- lm(log(y) ~ x1 + x2, data=twelve.train)
twelve.fit.rlm.exp <- rlm(log(y) ~ x1 + x2, data=twelve.train)
twelve.fit.lm.power <- lm(log(y) ~ log(x1) + log(x2), data=twelve.train)
twelve.fit.rlm.power <- rlm(log(y) ~ log(x1) + log(x2), data=twelve.train)

print(paste("validate data", Sys.time()))

#val$delta_12_6_prod_boe = val$first_12_month_prod_boe - val$first_6_month_prod_boe
#val$delta_6_3_prod_boe = val$first_6_month_prod_boe - val$first_3_month_prod_boe

#twelve.val <- data.frame(y=val$delta_12_6_prod_boe, x1=val$delta_6_3_prod_boe, x2=val$first_3_month_prod_boe)

# legal
twelve.val = twelve.val[twelve.val$x1 < twelve.x1.q,]
twelve.val = twelve.val[twelve.val$x2 < twelve.x2.q,]

# illegal
#twelve.val = twelve.val[twelve.val$y < q,]

twelve.predict.lm.linear = predict(twelve.fit.lm.linear, twelve.val)
twelve.predict.rlm.linear = predict(twelve.fit.rlm.linear, twelve.val)
twelve.predict.rf.linear = predict(twelve.fit.rf.linear, twelve.val)
twelve.predict.lm.mixed = predict(twelve.fit.lm.mixed, twelve.val)
twelve.predict.rlm.mixed = predict(twelve.fit.rlm.mixed, twelve.val)
twelve.predict.rf.mixed = predict(twelve.fit.rf.mixed, twelve.val)
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
twelve.err.rf.linear = twelve.predict.rf.linear - twelve.val$y
twelve.err.lm.mixed = twelve.predict.lm.mixed - twelve.val$y
twelve.err.rlm.mixed = twelve.predict.rlm.mixed - twelve.val$y
twelve.err.rf.mixed = twelve.predict.rf.mixed - twelve.val$y
twelve.err.lm.inter = twelve.predict.lm.inter - twelve.val$y
twelve.err.rlm.inter = twelve.predict.rlm.inter - twelve.val$y
twelve.err.lm.poly = twelve.predict.lm.poly - twelve.val$y
twelve.err.rlm.poly = twelve.predict.rlm.poly - twelve.val$y
twelve.err.lm.exp = (twelve.predict.lm.exp) - twelve.val$y
twelve.err.rlm.exp =(twelve.predict.rlm.exp) - twelve.val$y
twelve.err.lm.power = exp(twelve.predict.lm.power) - twelve.val$y
twelve.err.rlm.power = exp(twelve.predict.rlm.power) - twelve.val$y

Form = c("Linear", "", "", "Mixed", "", "", "Interaction", "", "Polynomial (3)", "", "Exponential", "", "Power", "")
Algo = c("lm","rlm","rf", "lm","rlm","rf", "lm","rlm","lm","rlm","lm","rlm","lm","rlm")
RootMeanSquareError = c(
rmse(twelve.err.lm.linear),
rmse(twelve.err.rlm.linear),
rmse(twelve.err.rf.linear),
rmse(twelve.err.lm.mixed),
rmse(twelve.err.rlm.mixed),
rmse(twelve.err.rf.mixed),
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
mae(twelve.err.rf.linear),
mae(twelve.err.lm.mixed),
mae(twelve.err.rlm.mixed),
mae(twelve.err.rf.mixed),
mae(twelve.err.lm.inter),
mae(twelve.err.rlm.inter),
mae(twelve.err.lm.poly),
mae(twelve.err.rlm.poly),
mae(twelve.err.lm.exp),
mae(twelve.err.rlm.exp),
mae(twelve.err.lm.power),
mae(twelve.err.rlm.power))

df = data.frame(Form, Algo, RootMeanSquareError, MeanAbsoluteError)
print(df)
smoothScatter(fitted(twelve.fit.rlm.power), residuals(twelve.fit.rlm.power))

print(paste("finish", Sys.time()))
