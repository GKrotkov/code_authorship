tp <- read.csv('turkpass.rin', sep='|', header=FALSE, col.names=c('pass', 'taxon'));

summary(glm(pass ~ factor(taxon), data=tp, family=binomial(logit)));
summary(glm(pass ~ I(taxon=='simple table'), data=tp, family=binomial(logit)));


tp$st <- tp$taxon == 'simple table';
st <- tp[tp$st,];
#st;

tp$ll <- tp$taxon == 'simple list' | tp$taxon == 'form list' | tp$taxon == 'text list' | tp$taxon == 'blog list';
ll <- tp[tp$ll,];
#ll;

tp$gg <-  tp$taxon == 'simple grid' | tp$taxon == 'form grid';
gg <- tp[tp$gg,];
#gg;

rr <- tp[! (tp$st | tp$ll | tp$gg),];
rr;

t.test(st$pass, ll$pass);
t.test(st$pass, tp[! tp$st,]$pass);
t.test(st$pass, c(ll$pass,gg$pass));
t.test(st$pass, gg$pass);
t.test(ll$pass, gg$pass);
 
summary(glm(pass ~ st + (ll | gg), data=tp, family=binomial(logit)));
summary(glm(pass ~ st + (ll | gg), data=tp, family=binomial));
glm.st <- glm(pass ~ st, data=tp, family=binomial);
summary(glm(pass ~ ll, data=tp, family=binomial));
summary(glm(pass ~ (ll|gg), data=tp, family=binomial));
glm.gg <- glm(pass ~ gg, data=tp, family=binomial);
glm.1 <- glm(pass ~ 1, data=tp, family=binomial);

summary(glm.st);
1-pchisq(glm.st$null.deviance - glm.st$deviance, glm.st$df.null - glm.st$df.residual);

summary(glm.gg);
1-pchisq(glm.gg$null.deviance - glm.gg$deviance, glm.gg$df.null - glm.gg$df.residual);

