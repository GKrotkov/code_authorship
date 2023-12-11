mtt <- read.table('mturktime.rin', sep='|');

mttt <- mtt[,3]


hist(mttt);

sum(mttt);

mean(mttt);
sd(mttt);
var(mttt);
mean(mttt) + c(-1,1)*sd(mttt);

quantile(mttt,probs=1:100/100);

length(mttt);