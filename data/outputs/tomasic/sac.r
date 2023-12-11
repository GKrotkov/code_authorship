sac <- read.table('sac.rin', sep='|', header=TRUE);
tax <- read.table('taxons.rin', sep='|', header=TRUE);
tax1 <- read.table('taxons1.rin', sep='|', header=TRUE);

# tax1$fi[1] = -8 + tax1$fi[1];

n <- nrow(sac);
n;

tax$p <- tax$mult / sum(tax$mult);

sac$rownum <- 1:n;

sum(sac$novel[sac$recordnum < 50]);

obs = c();
for (i in 1:nrow(sac)) {
  obs <- c(obs, sum(sac$novel[1:i]));
}

sac$novel;

plot(sac$rownum, obs);

c <- nrow(tax);

1:1;

Chat <- 1-tax1$fi[1]/n;
Chat;

cbind(tax1$fi, tax1$mult, tax1$mult-1, tax1$fi*tax1$mult*(tax1$mult-1));

g2hnum <- c * sum(tax1$fi*tax1$mult*(tax1$mult-1));
g2hdenom <- Chat * n * (n-1);

g2h <- pmax(-1+(g2hnum / g2hdenom), 0);

c(g2hnum,g2hdenom,g2h);

g2h;

Nhat <- 1/Chat * (c + n * (1-Chat)*g2h);

Nhat;
c(Nhat,c,Nhat-c);

f0 <- Nhat-c;

m <- 500*(1:20);

Nshen <- f0 * (1 - (1 - ((1-Chat)/f0))^m);

cbind(m,Nshen);

Shat <- c + (tax1$fi[1]^2 / (2*tax1$fi[2]));
Shat;

c + ((tax1$fi[1]*tax1$fi[1]-1) / (2*tax1$fi[2]+2));

