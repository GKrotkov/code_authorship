
mult <- 2.0;

mud_denom <- 65000000;
rdf_denom <- 37000000;
frb_denom <- 45000000;

mud <- read.table('wdc_microdata.psv', header=TRUE, sep="|", as.is=TRUE);
rdf <- read.table('wdc_rdfa.psv', header=TRUE, sep="|", as.is=TRUE);
frb <- read.table('freebase.psv', header=TRUE, sep="|", as.is=TRUE);

cbind(mud$Type,mud$Entities / mud_denom);
cbind(rdf$Type,rdf$Entities / rdf_denom);
cbind(frb$Domain,frb$Topics / frb_denom);
#q();

png('odesk.png', width=480,height=300);
par(mar=c(3.5,8,1.5,1));
barplot(rbind(mud$Entities / mud_denom, mud$odesk / 56), beside=TRUE, names.arg=mud$Type, horiz=TRUE, las=1, xlab='', legend.text=c('microdata','odesk'),args.legend=list(x="right"),main="Distribution of microdata versus ODesk tasks");
mtext('density',side=1,line=2);
sum(mud$odesk);

write.table(round(100*cbind(mud$Entities / mud_denom, mud$odesk / 56)), 'odesk.rout', row.names=mud$Type, col.names=c('microdata', 'odesk'));

q();

frb_busp <- (frb$Topics[8] + frb$Topics[10]) / frb_denom;

frb_busp;

ps <- c();
ts <- c();
dfs <- c();

odesk_bus <- c(rep(1,mult*15),rep(0,mult*(56-15)));

odesk_bus;

frbN <- 100;

for (i in 1:10000) {
  frb_bus <- rbinom(1, frbN, prob=frb_busp);
  frb_bus;

  tt <- t.test(odesk_bus, c(rep(1,frb_bus),rep(0,frbN-frb_bus)), var.equal=FALSE);
  ps <- c(ps, tt$p.value);
  ts <- c(ts, tt$statistic);
  dfs <- c(dfs, tt$parameter);
}

c(max(ps), range(ps),mean(ps));
c(max(ts), range(ts),mean(ts));
c(range(dfs), mean(dfs));

tci <- mean(ts) + c(-2,2)*sd(ts);
tci;
rbind(tci, 1-pt(tci,df=min(dfs)));
rbind(tci, 1-pt(tci,df=mean(dfs)));
rbind(tci, 1-pt(tci,df=max(dfs)));

mud_busp <- (mud$Entities[10]) / mud_denom;

mud_busp;

ps <- c();
ts <- c();
dfs <- c();

mudN <- 100;

for (i in 1:10000) {
  mud_bus <- rbinom(1, mudN, prob=mud_busp);
  mud_bus;

  tt <- t.test(odesk_bus, c(rep(1,mud_bus),rep(0,mudN-mud_bus)), var.equal=FALSE);
  ps <- c(ps, tt$p.value);
  ts <- c(ts, tt$statistic);
  dfs <- c(dfs, tt$parameter);
}

c(max(ps), range(ps),mean(ps));
c(max(ts), range(ts),mean(ts));
c(range(dfs), mean(dfs));

tci <- mean(ts) + c(-2,2)*sd(ts);
tci;
rbind(tci, 1-pt(tci,df=min(dfs)));
rbind(tci, 1-pt(tci,df=mean(dfs)));
rbind(tci, 1-pt(tci,df=max(dfs)));

rdf_busp <- (rdf$Entities[3] + rdf$Entities[17]) / rdf_denom;

rdf_busp;

ps <- c();
ts <- c();
dfs <- c();

rdfN <- 100;

for (i in 1:10000) {
  rdf_bus <- rbinom(1, rdfN, prob=rdf_busp);
  rdf_bus;

  tt <- t.test(odesk_bus, c(rep(1,rdf_bus),rep(0,rdfN-rdf_bus)), var.equal=FALSE);
  ps <- c(ps, tt$p.value);
  ts <- c(ts, tt$statistic);
  dfs <- c(dfs, tt$parameter);
}

c(max(ps), range(ps),mean(ps));
c(range(dfs), mean(dfs));

tci <- mean(ts) + c(-2,2)*sd(ts);
tci;
rbind(tci, 1-pt(tci,df=min(dfs)));
rbind(tci, 1-pt(tci,df=mean(dfs)));
rbind(tci, 1-pt(tci,df=max(dfs)));
