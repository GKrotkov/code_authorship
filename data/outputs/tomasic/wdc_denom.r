
mud <- read.table('wdc_microdata.psv', header=TRUE, sep="|");

mud;
mud$rank = 1:nrow(mud);

plot(mud$rank, mud$Entities);
plot(mud$rank, log10(mud$Entities));

mud2 <- mud[mud$rank>5,];

plot(mud2$rank, mud2$Entities);
plot(mud2$rank, log10(mud2$Entities));

lf1 <- lm(log10(mud$Entities)~mud$rank);
lf2 <- lm(log10(mud2$Entities)~mud2$rank);

summary(lf1);
summary(lf2);

b1 <- lf1$coefficients[[1]];
a1 <- lf1$coefficients[[2]];

mud3 <- cbind(1:200, c(mud$Entities, 10^(b1 + a1*21:200)));

mud3;

plot(mud3[,1], mud3[,2], log="y");
plot(mud3[,1], log(mud3[,2]));

b2 <- lf2$coefficients[[1]];
a2 <- lf2$coefficients[[2]];

mud4 <- cbind(1:200, c(mud$Entities, 10^(b2 + a2*21:200)));

mud4;

plot(mud4[,1], mud4[,2], log="y");
plot(mud4[,1], log(mud4[,2]));

sums3 <- c(sum(mud$Entities), sum(mud3[,2]));
c(sums3, sum(c(-1,+1)*sums3));
sums4 <- c(sum(mud$Entities), sum(mud4[,2]));
c(sums4, sum(c(-1,+1)*sums4));
