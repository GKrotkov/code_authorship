gr <- read.csv("grades.rin", header=T, sep='|');
sgr <- read.csv("survgrades.rin", header=T, sep='|');

sgrprog <- sgr[!!(sgr$programmer),];
sgrtab <- sgr[!!(sgr$taxon=='simple table'),];
sgrnontab <- sgr[!(sgr$taxon=='simple table'),];

sgrmult <- sgr[!!(sgr$multi=='true'),];
sgrnest <- sgr[!!(sgr$nesting=='true'),];
sgrexotic <- sgr[!!(sgr$multi=='true' | sgr$nesting=='true'),];

gr$grade

gr$rgrade <- 1*(gr$grade > 0.0 & gr$grade < 3.0) + 2*(gr$grade >= 3.0);

gr$rgrade;

hist(gr$rgrade,breaks=3);

stem(gr$grade)
hist(pmax(0,gr$grade),breaks=6)

mean(gr$rgrade);

sum(!!(gr$rgrade == 0))/length(gr$rgrade);
sum(!!(gr$rgrade == 1))/length(gr$rgrade);
sum(!!(gr$rgrade == 2))/length(gr$rgrade);

summary(glm((sgr$grade <= 0.0) ~ sgr$programmer, family=binomial));
summary(glm((grade >= 1.0) ~ programmer * I(taxon=='simple table'), data=sgr, family=binomial));
summary(glm((grade >= 1.0) ~ programmer * I(taxon=='simple list' | taxon=='form list'), data=sgr, family=binomial));
summary(glm((grade >= 2.0) ~ programmer * I(taxon=='simple table'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ programmer * I(taxon=='simple table'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ programmer * I(taxon=='simple list' | taxon=='form list'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ I(taxon=='simple table'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ I(taxon=='simple table') + I(taxon=='simple list' | taxon=='form list') + I(taxon=='simple grid' | taxon=='form grid'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ I(taxon=='simple list' | taxon=='form list') + I(taxon=='simple grid' | taxon=='form grid'), data=sgr, family=binomial));
summary(glm((grade >= 3.0) ~ I(taxon=='simple grid' | taxon=='form grid'), data=sgr, family=binomial));
summary(glm((sgr$grade >= 2.0) ~ sgr$programmer, family=binomial));
summary(glm((sgr$grade >= 3.0) ~ sgr$programmer, family=binomial));
summary(glm((grade >= 4.0) ~ programmer + I(taxon=='simple table'), family=binomial, data=sgr));

t.test((sgr$grade > 0.0)[!!sgr$programmer],(sgr$grade > 0.0)[!sgr$programmer]);
t.test((gr$grade > 0.0)[!!(gr$taxon=='simple table')],(gr$grade > 0.0)[!(gr$taxon == 'simple table')]);
t.test((gr$grade > 0.0)[!!(gr$taxon=='simple list' | gr$taxon=='form list')],(gr$grade > 0.0)[!(gr$taxon=='simple list' | gr$taxon=='form list')]);
t.test((gr$grade > 0.0)[!!(gr$taxon=='simple grid' | gr$taxon=='form grid')],(gr$grade > 0.0)[!(gr$taxon=='simple grid' | gr$taxon=='form grid')]);
t.test((sgr$grade >= 1.0)[!!sgr$programmer],(sgr$grade >= 1.0)[!sgr$programmer]);
t.test((sgr$grade > 1.0)[!!sgr$programmer],(sgr$grade > 1.0)[!sgr$programmer]);
t.test((gr$grade >= 1.0)[!!(gr$taxon=='simple table')],(gr$grade >= 1.0)[!(gr$taxon == 'simple table')]);
t.test((gr$grade >= 1.0)[!!(gr$taxon=='simple list' | gr$taxon=='form list')],(gr$grade >= 1.0)[!(gr$taxon=='simple list' | gr$taxon=='form list')]);
t.test((gr$grade >= 1.0)[!!(gr$taxon=='simple grid' | gr$taxon=='form grid')],(gr$grade >= 1.0)[!(gr$taxon=='simple grid' | gr$taxon=='form grid')]);
t.test((sgr$grade >= 3.0)[!!sgr$programmer],(sgr$grade >= 3.0)[!sgr$programmer]);
t.test((gr$grade >= 3.0)[!!(gr$taxon=='simple table')],(gr$grade >= 3.0)[!(gr$taxon == 'simple table')]);
t.test((gr$grade >= 3.0)[!!(gr$taxon=='simple list' | gr$taxon=='form list')],(gr$grade >= 3.0)[!(gr$taxon=='simple list' | gr$taxon=='form list')]);
t.test((gr$grade >= 3.0)[!!(gr$taxon=='simple grid' | gr$taxon=='form grid')],(gr$grade >= 3.0)[!(gr$taxon=='simple grid' | gr$taxon=='form grid')]);
t.test((gr$grade >= 4.0)[!!(gr$taxon=='simple table')],(gr$grade >= 4.0)[!(gr$taxon == 'simple table')]);
t.test((gr$grade >= 4.0)[!!(gr$taxon=='simple list' | gr$taxon=='form list')],(gr$grade >= 4.0)[!(gr$taxon=='simple list' | gr$taxon=='form list')]);
t.test((gr$grade >= 4.0)[!!(gr$taxon=='simple grid' | gr$taxon=='form grid')],(gr$grade >= 4.0)[!(gr$taxon=='simple grid' | gr$taxon=='form grid')]);

t.test((sgrtab$grade >= 4.0)[!!sgr$programmer],(sgrtab$grade >= 4.0)[!sgr$programmer]);
t.test((sgrnontab$grade >= 4.0)[!!sgrnontab$programmer],(sgrnontab$grade >= 4.0)[!sgrnontab$programmer],conf.level=0.35);
t.test((sgr$grade > 0.0)[!!sgr$programmer],(sgr$grade > 0.0)[!sgr$programmer]);
t.test((sgr$grade >= 4.0)[!!sgr$programmer],(sgr$grade >= 4.0)[!sgr$programmer]);
t.test((sgrexotic$grade >= 4.0)[!!sgrexotic$programmer],(sgrexotic$grade >= 4.0)[!sgrexotic$programmer],conf.level=0.35);
t.test((sgrmult$grade >= 4.0)[!!sgrmult$programmer],(sgrmult$grade >= 4.0)[!sgrmult$programmer]);
#t.test((sgrnest$grade >= 4.0)[!!sgrnest$programmer],(sgrnest$grade >= 4.0)[!sgrnest$programmer]);

mean(sgrnontab$grade >= 4.0);
mean((sgrnontab$grade >= 4.0)[!!sgrnontab$programmer]);
mean((sgrnontab$grade >= 4.0)[ !sgrnontab$programmer]);
length((sgrexotic$grade >= 4.0)[!!sgrexotic$programmer]);
length((sgrexotic$grade >= 4.0)[ !sgrexotic$programmer]);
length((sgrnest$grade >= 4.0)[!!sgrnest$programmer]);
length((sgrnest$grade >= 4.0)[ !sgrnest$programmer]);
length((sgrmult$grade >= 4.0)[!!sgrmult$programmer]);
length((sgrmult$grade >= 4.0)[ !sgrmult$programmer]);
