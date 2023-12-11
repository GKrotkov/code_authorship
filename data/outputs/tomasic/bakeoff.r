eval <- read.table('unpaired.rin', sep='|', header=TRUE,colClasses=c('numeric','numeric','numeric','factor'));

algs <- read.table('algs.rin',as.is=TRUE)[,1];
algs;

length(algs);

bb <- read.table('bakeoff.rin', sep='|', header=TRUE,colClasses=c('numeric','numeric','numeric','factor'));

#levels(bb$code)[bb$code];

balgs  <- c("reform","bernsvm.5.0","propsvm.0.0","libsvm","conjheur","soupheur","heur");
balgs2 <- c("reform","bernsvm.5.0*","propsvm.0.0*","libsvm","conjheur*","soupheur","heur*");

bb$code1 <- ordered(bb$code, balgs);
code1 <- ordered(bb$code, balgs);

par()$mar

par(mar=2*par()$mar);

boxplot(f1~code1, data=bb, las=2, names=balgs2);
#plot(bb$f1~code1);

png('bakeoff.png');
par(mar=2*par()$mar);
boxplot(f1~code1, data=bb, las=2, names=balgs2);


for (i in 1:(length(algs)-1)) {
  for (j in (i+1):length(algs)) {
    ai <- algs[i];
    aj <- algs[j];

    print(c('TTT',i,j,ai,aj));

    ee <- eval[(eval$code == ai)|(eval$code == aj),];
    tt <- t.test(f1~code, data=ee);
    print(tt);

    delta <- sum(c(1,-1)*tt$estimate);

    if (tt$p.value < 0.05) {
      print(c('SIG',ai,sign(delta),aj));
    }   
  }
}
