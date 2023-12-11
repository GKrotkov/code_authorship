zz <- read.table('zipf.rin', sep='|', head=T);

#zz;

sum(zz$goodhits);
sum(zz$hits);

cbind(zz$goodhits,cumsum(zz$goodhits),cumsum(zz$goodhits)/sum(zz$goodhits));
cbind(zz$hits,cumsum(zz$hits),cumsum(zz$hits)/sum(zz$hits));

median(zz$goodhits);

hist(zz$tables,breaks=20);
hist(zz$hits,breaks=8);
hist(zz$goodhits,breaks=8);
for (b in 0:5) {
  #hist(zz$hits,breaks=2^b);
}