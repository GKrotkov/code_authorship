by(train, train$well_id, function (x) my(x))
my <- function (y) for (a in y){ print(a); print(a) }
d <- function (wid, lat, lon) t <- (train$latitude-lat)^2 + (train$longitude-lon)^2; 
t <- data.frame(train$well_id, train$completion_id, train$latitude, train$longitude, train$first_3_month_prod_boe, d(train$latitude, train$longitude))

r <- apply(train[,c('well_id', 'latitude','longitude')], 1, function (x) d(x[1],x[2],x[3]))

train[which.min(train$latitude),c('well_id','latitude'),]

z <- function (wid, lat, lon) apply(t[,c('well_id', 'latitude','longitude')], 1, function (train$latitude-lat)^2 + (train$longitude-lon)^2)

 with(train, (latitude-lat)^2 + (longitude-lon)^2)

t[which.min(z(100,100)),c('well_id','latitude'),]


for (e in t) {
  t$t <- 

# produces the row with the minimum distance

z <- function (i,lat, lon) with(t, 100000*(i==well_id) + (latitude-lat)^2 + (longitude-lon)^2)
w <- function (i,x,y) t[which.min(z(i,x,y)),c('well_id')]

t$nearest <- apply(t[,c('well_id', 'latitude', 'longitude')], 1, function(x) w(x[1],x[2],x[3]))

# expanded model

z <- function (i,lat, lon) with(t, 100000*(i==well_id) + (latitude-lat)^2 + (longitude-lon)^2)
w <- function (i,x,y) t[which.min(z(i,x,y)),c('well_id', 'latitude', 'longitude', 'first_3_month_prod_boe')]

t$nearest <- apply(t[,c('well_id', 'latitude', 'longitude')], 1, function(x) w(x[1],x[2],x[3]))
