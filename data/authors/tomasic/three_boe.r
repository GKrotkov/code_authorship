# this file takes well_completion as input and predicts three month boe

source("setup.r")

print(paste("fit data", Sys.time()))

three <- data.frame(
#  well_id=data$well_id,
  latitude=data$latitude, 
  longitude=data$longitude, 
  country=data$country, # significant alone, but next makes insignificant
  state_province=data$state_province, # significant
  rs_region=data$rs_region, # significant, interacts with above
  rs_basin=data$rs_basin, # log(nn) + significant .496
#  rs_play=data$rs_play, 
#  rs_sub_play=data$rs_sub_play, 
  trajectory=data$trajectory, # log(nn) + significant .4799
  vintage=data$vintage, # log(nn) + .468
  completion_date_timestamp=data$completion_date_timestamp,
  rs_frac_job_type=data$rs_frac_job_type, 
#  rs_fluid_type=data$rs_fluid_type, 
  completion_design=data$completion_design,
  rs_proppant_type=data$rs_proppant_type, 
  first_3_month_prod_boe=data$first_3_month_prod_boe
) # log(y) ~ log(nn) 0.4678 

print(nrow(data))
print(nrow(three))
# FIXME - the cut off points should be determined automagically
three.train <- three[data$completion_date < "2010-01-01",]
three.val <- three[data$completion_date >= "2010-01-01" & data$completion_date < "2013-04-01",]
three.test <- three[data$completion_date >= "2013-04-01",]

print(nrow(three.train))
print(nrow(three.val))
print(nrow(three.test))


# three.train.first_3_month_prod_boe.q = quantile(three.train$first_3_month_prod_boe, prob=0.99)
# three.train = three.train[three.train$first_3_month_prod_boe < three.train.first_3_month_prod_boe.q, ]
three.t <- data.frame(three.train$latitude, three.train$longitude)
three.u <- get.knn(three.t, k=20)
#three.train$nn.index <- three.u$nn.index
#three.train$nn.dist <- three.u$nn.dist
# three.u$nn.index - index of row nearest (three columns)
# three.u$nn.dist - distance for that row 

print(paste("scan training nn data", Sys.time()))

# FIXME slow - 10 minutes - since its not the R way
# FIXME turn into a function

# i tracks each row of the dataset
# t,u records the index and distance of nn
# k is the birth date of the well
# j scans the nn of row i until finding a match older than i
l <- length(three.u$nn.index[,1])
ind <- integer(l)
dis <- integer(l)
for (i in 1:l) {
  t <- NA;
  u <- NA;
  for (j in 1:length(three.u$nn.index[1,])) {
    k <- three.train[three.u$nn.index[i,j], 'completion_date_timestamp'];
    if (is.na(t) && three.train[i,'completion_date_timestamp'] < k) {
      t <- three.u$nn.index[i,j]
      u <- three.u$nn.dist[i,j]
      break
    }
  };
  ind[i] <- t;
  dis[i] <- u;
}

# ind may have na values

#three.train$nearest_first_3_month_prod_boe <- three.train[three.u$nn.index[,1],'first_3_month_prod_boe']
three.train$nearest_first_3_month_prod_boe <- three.train[ind,'first_3_month_prod_boe']
three.train$dist_nearest_first_3_month_prod_boe <- dis

print(nrow(three.train))

print(sum(is.na(three.train$nearest_first_3_month_prod_boe)))
print(sum(is.na(three.train$dist_nearest_first_3_month_prod_boe)))

print(nrow(three.train))
three.train <- three.train[complete.cases(three.train),]
print(nrow(three.train))

print(sum(is.na(three.train$nearest_first_3_month_prod_boe)))
print(sum(is.na(three.train$dist_nearest_first_3_month_prod_boe)))

print(nrow(three.train))

# FIXME - don't sample from wells far away
# FIXME - add distance of nearest well as a feature
# FIXME - neural network MLP, random forest
# FIXME

three.fit.lm.linear <- lm(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe, data=three.train)
three.fit.rlm.linear <- rlm(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe, data=three.train, maxit=40)
three.fit.lm.power <- lm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe), data=three.train)
three.fit.rlm.power <- rlm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe), data=three.train, maxit=40)
three.fit.lm.mixed <- lm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe) + rs_basin + trajectory + vintage, data=three.train)
three.fit.rlm.mixed <- rlm(log(first_3_month_prod_boe) ~ log(nearest_first_3_month_prod_boe) + rs_basin + trajectory + vintage, data=three.train, maxit=40)
# FIXME - na.action=na.exclude not necessary since no NA exist in dataset
three.fit.rf.mixed <- randomForest(first_3_month_prod_boe ~ nearest_first_3_month_prod_boe + rs_basin + trajectory + vintage, data=three.train, ntree=10, maxnodes=150, na.action=na.exclude)

# use the training distribution to trim the validation distribution
#three.val = three.val[three.val$first_3_month_prod_boe < three.train.first_3_month_prod_boe.q, ]

three.val$nearest_first_3_month_prod_boe <- NA
three.val$dist_nearest_first_3_month_prod_boe <- NA
three.trainval <- as.data.frame(rbind(three.train, three.val))

# need to do nn again over combined training and validation set
three.tnv <- as.data.frame(rbind(data.frame(latitude=three.train$latitude, longitude=three.train$longitude), data.frame(latitude=three.val$latitude, longitude=three.val$longitude)))
three.tnv <- get.knn(three.tnv, k=10)
# $nn.index
# $nn.dist

# FIX ME - this should start at the first validation row, not the entire table

print(paste("scan val nn data", Sys.time()))

# i tracks each row of the dataset
# t,u records the index and distance of nn
# k is the birth date of the well
# j scans the nn of row i until finding a match older than i
el <- length(three.tnv$nn.index[,1])
t <- integer(1)
u <- integer(1)
ind <- integer(el)
dis <- integer(el)
for (i in 1:el) {
  t <- NA;
  u <- NA;
  for (j in 1:length(three.tnv$nn.index[i,])) {
    k <- three.trainval[three.tnv$nn.index[i,j], 'completion_date_timestamp'];
    if (is.na(t) && three.trainval[i,'completion_date_timestamp'] < k) {
      t <- three.tnv$nn.index[i,j]
      u <- three.tnv$nn.dist[i,j]
      break
    }
  };
  ind[i] <- t;
  dis[i] <- u;
}

# ind contains NA

# this indirect assignment doesn't work quite right
three.trainval$nearest_first_3_month_prod_boe <- three.trainval[ind,'first_3_month_prod_boe']
three.trainval$dist_nearest_first_3_month_prod_boe <- dis

a = length(three.train[,1])+1
b = length(three.trainval[,1])
three.val <- three.trainval[a:b,]


print(a)
print(b)
print(nrow(three.val))

three.val <- three.val[complete.cases(three.val),]
print(nrow(three.val))

# FIXME also estimate those wells without nn

three.predict.lm.linear = predict(three.fit.lm.linear, three.val)
three.predict.rlm.linear = predict(three.fit.rlm.linear, three.val)
three.predict.lm.power = predict(three.fit.lm.power, three.val)
three.predict.rlm.power = predict(three.fit.rlm.power, three.val)
three.predict.lm.mixed = predict(three.fit.lm.mixed, three.val)
three.predict.rlm.mixed = predict(three.fit.rlm.mixed, three.val)
three.predict.rf.mixed = predict(three.fit.rf.mixed, three.val)


three.err.lm.linear = three.predict.lm.linear - three.val$first_3_month_prod_boe
three.err.rlm.linear = three.predict.rlm.linear - three.val$first_3_month_prod_boe
three.err.lm.power = exp(three.predict.lm.power) - three.val$first_3_month_prod_boe
three.err.rlm.power = exp(three.predict.rlm.power) - three.val$first_3_month_prod_boe
three.err.lm.mixed = exp(three.predict.lm.mixed) - three.val$first_3_month_prod_boe
three.err.rlm.mixed = exp(three.predict.rlm.mixed) - three.val$first_3_month_prod_boe
three.err.rf.mixed = three.predict.rf.mixed - three.val$first_3_month_prod_boe

Form = c("Linear", "", "Power", "", "Mixed", "", "")
Algo = c("lm","rlm","lm","rlm","lm","rlm", "rf")

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
mae(three.err.rlm.mixed),
mae(three.err.rf.mixed))

df = data.frame(Form, Algo, RootMeanSquareError, MeanAbsoluteError)
print(df)
smoothScatter(fitted(three.fit.rlm.mixed), residuals(three.fit.rlm.mixed))

print(paste("finish", Sys.time()))
