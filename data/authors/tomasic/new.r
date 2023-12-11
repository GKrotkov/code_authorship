#

train <- read.csv("well_completion_trim_train.csv", header=T)

library(FNN) # knn
library(MASS) # rlm
library(e1071) # kurtosis
library(randomForest)
library(RSNNS) # mlp
# FIXME - there should be no NA for latitude, longitude from SQL dump

three.train <- data.frame(
well_id=train$well_id,
latitude=train$latitude, 
longitude=train$longitude, 
country=train$country, # significant alone, but next makes insignificant
state_province=train$state_province, # significant
rs_region=train$rs_region, # significant, interacts with above
rs_basin=train$rs_basin, # log(nn) + significant .496
rs_play=train$rs_play, 
rs_sub_play=train$rs_sub_play, 
trajectory=train$trajectory, # log(nn) + significant .4799
vintage=train$vintage, # log(nn) + .468
completion_date_timestamp=train$completion_date_timestamp,
rs_frac_job_type=train$rs_frac_job_type, 
rs_fluid_type=train$rs_fluid_type, 
completion_date_timestamp=train$completion_date_timestamp,
completion_design=train$completion_design,
rs_proppant_type=train$rs_proppant_type, 
first_3_month_prod_boe=train$first_3_month_prod_boe
) # log(y) ~ log(nn) 0.4678 
three.train <- three.train[(!is.na(three.train$latitude)) & (!is.na(three.train$longitude)),]
three.train.first_3_month_prod_boe.q = quantile(three.train$first_3_month_prod_boe, prob=0.99)
three.train = three.train[three.train$first_3_month_prod_boe < three.train.first_3_month_prod_boe.q, ]
three.t <- data.frame(three.train$latitude, three.train$longitude)
three.u <- get.knn(three.t, k=10)
three.train$nn.index <- three.u$nn.index
three.train$nn.dist <- three.u$nn.dist
# three.u$nn.index - index of row nearest (three columns)
# three.u$nn.dist - distance for that row 

# slow - 10 minutes - since its not the R way
l <- length(three.u$nn.index[,1])
ind <- integer(l)
dis <- integer(l)
for (i in 1:length(three.u$nn.index[,1])) {
  t <- NA;
  u <- NA;
  for (j in 1:length(three.u$nn.index[1,])) {
    k <- three.train[three.u$nn.index[i,j], 'completion_date_timestamp']
    if (is.na(t) && three.train[i,'completion_date_timestamp'] < k) {
      t <- three.u$nn.index[i,j]
      u <- three.u$nn.dist[i,j]
    }
  };
  ind[i] <- t;
  dis[i] <- u;
}


#three.train$nearest_first_3_month_prod_boe <- three.train[three.u$nn.index[,1],'first_3_month_prod_boe']
three.train$nearest_first_3_month_prod_boe <- three.train[ind,'first_3_month_prod_boe']
three.train$dist_nearest_first_3_month_prod_boe <- dis

# FIXME - don't sample from wells newer
# FIXME - don't sample from wells far away
# FIXME - add distance of nearest well as a feature
# FIXME - neural network MLP, random forest
# FIXME - discard negative completion_date_timestamp
# FIXME - fix random forest by using split function inside R instead outside

three.fit.lm.linear <- lm(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe, data=three.train)
three.fit.rlm.linear <- rlm(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe, data=three.train, maxit=40)
three.fit.lm.power <- lm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe), data=three.train)
three.fit.rlm.power <- rlm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe), data=three.train, maxit=40)
three.fit.lm.mixed <- lm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe) + rs_basin + trajectory + vintage, data=three.train)
three.fit.rlm.mixed <- rlm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe) + rs_basin + trajectory + vintage, data=three.train, maxit=40)
# three.fit.rf.mixed <- randomForest(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe + rs_basin + trajectory + vintage, data=three.train, maxnodes=100)

val <- read.csv("well_completion_trim_val.csv", header=T)

three.val <- data.frame(
latitude=val$latitude,
longitude=val$longitude,
rs_basin=val$rs_basin,
trajectory=val$trajectory,
vintage=val$vintage,
first_3_month_prod_boe=val$first_3_month_prod_boe
)
three.val <- three.val[(!is.na(three.val$latitude)) & (!is.na(three.val$longitude)),]

# use the training distribution to trim the validation distribution
three.val = three.val[three.val$first_3_month_prod_boe < three.train.first_3_month_prod_boe.q, ]

# this is wrong, since it does nearest neighbor only over the validation set,
# instead it should include the training set
three.vt <- data.frame(three.val$latitude, three.val$longitude)
three.vu <- get.knn(three.vt, k=3)
# t$nn.index
# t$nn.dist

three.val$nearest_first_3_month_prod_boe <- three.val[three.vu$nn.index[,1],'first_3_month_prod_boe']

three.predict.lm.linear = predict(three.fit.lm.linear, three.val)
three.predict.rlm.linear = predict(three.fit.rlm.linear, three.val)
three.predict.lm.power = predict(three.fit.lm.power, three.val)
three.predict.rlm.power = predict(three.fit.rlm.power, three.val)
three.predict.lm.mixed = predict(three.fit.lm.mixed, three.val)
three.predict.rlm.mixed = predict(three.fit.rlm.mixed, three.val)
# three.predict.rf.mixed = predict(three.fit.rf.mixed, three.val)


three.err.lm.linear = three.predict.lm.linear - three.val$first_3_month_prod_boe
three.err.rlm.linear = three.predict.rlm.linear - three.val$first_3_month_prod_boe
three.err.lm.power = exp(three.predict.lm.power) - three.val$first_3_month_prod_boe
three.err.rlm.power = exp(three.predict.rlm.power) - three.val$first_3_month_prod_boe
three.err.lm.mixed = exp(three.predict.lm.mixed) - three.val$first_3_month_prod_boe
three.err.rlm.mixed = exp(three.predict.rlm.mixed) - three.val$first_3_month_prod_boe
# three.err.rf.mixed = three.predict.rf.mixed - three.val$first_3_month_prod_boe

Form = c("Linear", "", "Power", "", "Mixed", "", "")
Algo = c("lm","rlm","lm","rlm","lm","rlm", rf)

rmse <- function(error)
{
     sqrt(mean(error^2))
}

mae <- function(error)
{
	mean(abs(error))
}

RootMeanSquareError = c(
rmse(three.err.lm.linear),
rmse(three.err.rlm.linear),
rmse(three.err.lm.power),
rmse(three.err.rlm.power),
rmse(three.err.lm.mixed),
rmse(three.err.rlm.mixed),
rmse(three.err.rf.mixed)
)

MeanAbsoluteError = c(
mae(three.err.lm.linear),
mae(three.err.rlm.linear),
mae(three.err.lm.power),
mae(three.err.rlm.power),
mae(three.err.lm.mixed),
mae(three.err.rlm.mixed))

df = data.frame(Form, Algo, RootMeanSquareError, MeanAbsoluteError)
df
smoothScatter(fitted(three.fit.rlm.mixed), residuals(three.fit.rlm.mixed))
