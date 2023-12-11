gsurv <- read.table('GradeSurvival.rin', header=TRUE, sep='|');
gsurv;

library(survival);

sm <- coxph(Surv(gradecount,highq) ~ 1+genus, data=gsurv);
summary(sm);


kmfit <- survfit(Surv(gradecount,highq) ~ 1 + genus, data=gsurv);
kmfit;
plot(kmfit);

expfit <- survreg(Surv(gradecount,highq) ~ 1 + genus, data=gsurv, dist="exponential");
expfit;
summary(expfit);
exp(-expfit$coeff);

dim(expfit$coeff);
names(expfit$coeff);

pg <- exp(-expfit$coeff[[1]]);
pl <- exp(-expfit$coeff[[1]] - expfit$coeff[[2]]);
pt <- exp(-expfit$coeff[[1]] - expfit$coeff[[3]]);

esucc <- function(p, n) {
  return (1 - (1-p)^n);
}

list(table=pt,list=pl,grid=pg);

rbind(1:10,esucc(pg,1:10));
esucc(pl,1:7);
esucc(pt,1:2);

q();

phfit <- coxph(Surv(gradecount,highq) ~ 1 + genus, data=gsurv);
summary(phfit);


survdiff(Surv(gradecount,highq) ~ as.factor(genus), data=gsurv);
survdiff(Surv(gradecount,highq) ~ genus, data=gsurv);
survdiff(Surv(gradecount,highq) ~ (genus == 'table'), data=gsurv);


summary(coxph(Surv(gradecount,highq) ~ 1, data=gsurv));
summary(coxph(Surv(gradecount,highq) ~ 1+(genus == 'table'), data=gsurv));
summary(coxph(Surv(gradecount,highq) ~ 1+(genus == 'list'), data=gsurv));
summary(coxph(Surv(gradecount,highq) ~ 1+(genus == 'grid'), data=gsurv));

q();

exp(coxph(Surv(gradecount,highq) ~ 1, data=gsurv)$loglik);

basehaz(coxph(Surv(gradecount,highq) ~ 1, data=gsurv));

#print(coxme(Surv(gradecount,highq) ~ (genus == 'table'), data=gsurv));
