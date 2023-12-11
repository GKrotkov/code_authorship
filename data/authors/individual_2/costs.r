cc <- read.table('costs.rin', sep='|', header=T);

plot(cc$tasks,cc$maxgrade);

cor(cc$tasks,cc$maxgrade);

sp <- read.csv("survpass.rin", header=T, sep='|');
sp[1:10,];
tp <- read.csv("turkpass.rin", header=T, sep='|');

gr <- read.csv("grades.rin", header=T, sep='|');
sgr <- read.csv("survgrades.rin", header=T, sep='|');

spr <- qnorm(0.95);
#spr <- qnorm(0.90);

ilogit <- function(lo) {
  return (exp(lo) / (1+exp(lo)));
}

summary(glm((rgrade >= 1)~I(multi == 'true'), data=sgr, family=binomial));
summary(glm((rgrade >= 2)~I(multi == 'true'), data=sgr, family=binomial));
summary(glm((rgrade >= 1)~I(multipage == 'true'), data=sgr, family=binomial));
summary(glm((rgrade >= 2)~I(multipage == 'true'), data=sgr, family=binomial));
summary(glm((rgrade >= 1)~I(nesting == 'true')*genus, data=sgr, family=binomial));
summary(glm((rgrade >= 2)~I(nesting == 'true')*genus, data=sgr, family=binomial));

out <- c();

for (spr in c(qnorm(0.95))) {
  for (gg in c('table', 'list', 'grid')) {
    dd <- list();
    dd$skipfit <- glm(class~I(genus==gg), data=sp, family=binomial);
      print(c('gg',gg));
      print(summary(dd$skipfit));
      dd$skipest <- predict(dd$skipfit,newdata=sp[sp$genus==gg,][1,],type='response',se.fit=TRUE);
      print(dd$skipest);
      dd$logskipest <- predict(dd$skipfit,newdata=sp[sp$genus==gg,][1,],se.fit=TRUE);

        dd$gradfit1 <- glm((rgrade >= 1) ~ I(genus==gg),data=sgr,family=binomial);    
        print(summary(dd$gradfit1));
        dd$gradest1 <- predict(dd$gradfit1,newdata=sgr[sgr$genus==gg,][1,],type='response',se.fit=TRUE);
        print(dd$gradest1);
        dd$loggradest1 <- predict(dd$gradfit1,newdata=sgr[sgr$genus==gg,][1,],se.fit=TRUE);
        dd$gradfit2 <- glm((rgrade >= 2) ~ I(genus==gg),data=sgr,family=binomial);
        print(summary(dd$gradfit2));
        dd$gradest2 <- predict(dd$gradfit2,newdata=sgr[sgr$genus==gg,][1,],type='response',se.fit=TRUE);
        print(dd$gradest2); 
        dd$loggradest2 <- predict(dd$gradfit2,newdata=sgr[sgr$genus==gg,][1,],se.fit=TRUE);

        print(c(gg,'N',dim(sgr[sgr$genus==gg,])));

        dd$notskip <- 1-dd$skipest$fit + spr*c(-1,1)*dd$skipest$se.fit;
        print(c(gg,'!skip',dd$notskip));
        dd$plaus <- dd$gradest1$fit + spr*c(-1,1)*dd$gradest1$se.fit
        print(c(gg,'plaus',dd$plaus));
        dd$highq <- pmax(0,dd$gradest2$fit + spr*c(-1,1)*dd$gradest2$se.fit);
        print(c(gg,'highq',dd$highq));

        dd$plausand <- dd$notskip * dd$plaus;
        print(c(gg,'plausand',dd$plausand));
        dd$highqand <- dd$notskip * dd$highq;
        print(c(gg,'highqand',dd$highqand));

        dd$plausneed <- 1/dd$plausand;
        print(c(gg,'plausneed',range(dd$plausneed)));
        dd$highqneed <- 1/dd$highqand;
        print(c(gg,'highqneed',range(dd$highqneed)));

        dd$notskip1 <- range(1-ilogit(dd$logskipest$fit + spr*c(-1,1)*dd$logskipest$se.fit));
        print(c(gg,'!skip1',dd$notskip1));
        dd$plaus1 <- range(ilogit(dd$loggradest1$fit + spr*c(-1,1)*dd$loggradest1$se.fit));
        print(c(gg,'plaus1',dd$plaus1));
        dd$highq1 <- pmax(0,range(ilogit(dd$loggradest2$fit + spr*c(-1,1)*dd$loggradest2$se.fit)));
        print(c(gg,'highq1',dd$highq1));

        dd$plausand1 <- dd$notskip1 * dd$plaus1;
        print(c(gg,'plausand1',dd$plausand1));
        dd$highqand1 <- dd$notskip1 * dd$highq1;
        print(c(gg,'highqand1',dd$highqand1));

        dd$plausneed1 <- 1/dd$plausand1;
        print(c(gg,'plausneed1',range(dd$plausneed1)));
        dd$highqneed1 <- 1/dd$highqand1;
        print(c(gg,'highqneed1',range(dd$highqneed1)));
 
        dd$highqcost1 <- 0.1 * c(floor(min(dd$highqneed1)),ceiling(max(dd$highqneed1))) + 0.3;
        print(c(gg,'highqcost1',range(dd$highqcost1)));
        dd$highqcost2 <- round(c(floor(min(dd$highqneed1)),ceiling(max(dd$highqneed1))) * (0.1 + 0.2 * dd$plausand1), digits=1);
        print(c(gg,'highqcost2',range(dd$highqcost2)));

        out <- rbind(out, list(genus=gg,
                         notskip=mean(dd$notskip1),notskip.se=max(dd$notskip1)-mean(dd$notskip1),
                         plaus=mean(dd$plaus1),plaus.se=max(dd$plaus1)-mean(dd$plaus1),
                         highq=mean(dd$highq1),highq.se=max(dd$highq1)-mean(dd$highq1),
                         plausand=mean(dd$plausand1),plausand.se=max(dd$plausand1)-mean(dd$plausand1),
                         highqand=mean(dd$highqand1),highqand.se=mean(dd$highqand1)-min(dd$highqand1),
                         plausneed=mean(dd$plausneed1),plausneed.se=max(dd$plausneed1)-mean(dd$plausneed1),
                         highqneed=mean(dd$highqneed1),highqneed.se=mean(dd$highqneed1)-min(dd$highqneed1),
                         highqcost=mean(dd$highqcost1),highqcost.se=mean(dd$highqcost1)-min(dd$highqcost1),
                         ubqcost=max(dd$highqcost1),
                         highqcost2=mean(dd$highqcost2),highqcost2.se=mean(dd$highqcost2)-min(dd$highqcost2),
                         ubqcost2=max(dd$highqcost2)));

  }
}

out;