ww <- read.table('volwhen.rin', sep='|', header=TRUE);

ww$num2 <- pmin(20.7, ww$num);

ww$x <- as.Date(ww$date, '%Y-%m-%d');

ww$x;

plot(spline(ww$x,ww$num2));

png('voltime.png', height=500, width=1500);
par(bg="transparent", fg="white");
par(las=1);
plot(ww$x, ww$num2, type='p', ylim=c(0,20), cex=2.0, ylab="", xlab="", main='Volunteer Submissions per day',col.axis="white",col.main="white", cex.axis=3.0, cex.main=3.0);

