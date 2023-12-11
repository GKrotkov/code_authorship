cc <- read.table('conv.psv', sep='|', header=TRUE, comment.char='\\');
cc;

plot(cc$gradecount, cc$phq);
plot(cc$conv, cc$reqs);
plot(cc$gradecount, cc$topgrade);
plot(cc$reqs, cc$topgrade);
plot(cc$conv, cc$topgrade);
abline(a=-4.47894736842105,b=3);
abline(a=-3.84210526315789,b=3);
plot(cc$gradecount, cc$meangrade);
plot(cc$conv, cc$meangrade);
plot(log(cc$conv), cc$topgrade);
plot(log(cc$conv), cc$meangrade);

