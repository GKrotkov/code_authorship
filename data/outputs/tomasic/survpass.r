sp <- read.csv("survpass.rin", header=T, sep='|');
sp[1:10,];
tp <- read.csv("turkpass.rin", header=T, sep='|');

stem(sp$class);
mean(sp$class);

fit1 <- glm(class~I(genus=='table'), data=sp, family=binomial);
summary(fit1);

predict(fit1,newdata=sp[sp$genus=='table',][1,],type='response',se.fit=TRUE);
confint(fit1);

summary(glm(class~I(genus=='grid'), data=sp, family=binomial));
summary(glm(class~I(genus=='list'), data=sp, family=binomial));
summary(glm(class~programmer*I(taxon=='simple list' | taxon=='form list'), data=sp, family=binomial));

for (gg in c('table', 'list', 'grid')) {
  mm <- glm(class~I(genus==gg), data=sp, family=binomial);
  print(c('gg',gg));
  print(summary(mm));
  print(predict(fit1,newdata=sp[sp$genus==gg,][1,],type='response',se.fit=TRUE));
}

summary(glm(class~I(taxon=='simple table'), data=tp, family=binomial));
summary(glm((1-class)~I(taxon=='simple table'), data=tp, family=binomial));
summary(glm(class~I(taxon=='simple table')+I(taxon=='simple grid' | taxon=='form grid'), data=tp, family=binomial));
summary(glm(class~I(taxon=='simple grid' | taxon=='form grid'), data=tp, family=binomial));
summary(glm(class~I(taxon=='simple list' | taxon=='form list'), data=tp, family=binomial));

fit2 <- glm(class~I(taxon=='simple table')+programmer, data=sp, family=binomial);
summary(fit2);

fit3 <- glm(class~I(taxon=='simple table')*programmer, data=sp, family=binomial);
summary(fit3);

fit4 <- glm(class~programmer, data=sp, family=binomial);
summary(fit4);

#cbind(sp$programmer,sp$class);

sp$class[!!sp$programmer];
sp$class[ !sp$programmer];

boxplot(sp$class[!!sp$programmer],sp$class[!sp$programmer]);
t.test(sp$class[!!sp$programmer],sp$class[!sp$programmer]);

t.test(sp$class[!!(sp$taxon=='simple table')],sp$class[ !(sp$taxon=='simple table')]);
t.test(sp$class[!!(sp$taxon=='simple list' | sp$taxon=='form list')],sp$class[ !(sp$taxon=='simple list' | sp$taxon=='form list')]);
t.test(sp$class[!!(sp$taxon=='simple grid' | sp$taxon=='form grid')],sp$class[ !(sp$taxon=='simple grid' | sp$taxon=='form grid')]);
