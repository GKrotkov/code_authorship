library('MultinomialCI');

alpha <- 0.05;

wm <- read.table('WrappableMoments.rin', header=T, sep='|');

gvt <- read.table('genusvotetally.rin', header=T, sep='|');

gd <- read.table('genusdist.rin', header=T, sep='|');

wm;
gvt;
gd;

pqm <- list();
pqm$grid <- multinomialCI(c(wm$nj[1],wm$n[1]-wm$nj[1]), alpha);
pqm$list <- multinomialCI(c(wm$nj[2],wm$n[2]-wm$nj[2]), alpha);
pqm$tabl <- multinomialCI(c(wm$nj[3],wm$n[3]-wm$nj[3]), alpha);

pqm;

pq <- list();
pq$grid <- mean(pqm$grid[1,]);
pq$list <- mean(pqm$list[1,]);
pq$tabl <- mean(pqm$tabl[1,]);

pq;

pqw <- list();
pqw$grid <- 0.5 * (pqm$grid[1,2] - pqm$grid[1,1]);
pqw$list <- 0.5 * (pqm$list[1,2] - pqm$list[1,1]);
pqw$tabl <- 0.5 * (pqm$tabl[1,2] - pqm$tabl[1,1]);

pqw;


pdm <- list();
pdm$grid <- multinomialCI(c(gvt$rawupvotes[1],gvt$rawdownvotes[1]), alpha);
pdm$list <- multinomialCI(c(gvt$rawupvotes[2],gvt$rawdownvotes[2]), alpha);
pdm$tabl <- multinomialCI(c(gvt$rawupvotes[3],gvt$rawdownvotes[3]), alpha);

pdm;

pd <- list();
pd$grid <- mean(pdm$grid[1,]);
pd$list <- mean(pdm$list[1,]);
pd$tabl <- mean(pdm$tabl[1,]);


pdw <- list();
pdw$grid <- 0.5 * (pdm$grid[1,2] - pdm$grid[1,1]);
pdw$list <- 0.5 * (pdm$list[1,2] - pdm$list[1,1]);
pdw$tabl <- 0.5 * (pdm$tabl[1,2] - pdm$tabl[1,1]);

pdw;

mtm <- multinomialCI(c(gd$num[1],gd$num[2],gd$num[3]), alpha);
mto <- multinomialCI(c(gd$num[7],gd$num[8],gd$num[9]), alpha);

mtm;
mto;

ptm <- list();
ptmw <- list();
ptmw$grid <- 0.5 * (mtm[1,2] - mtm[1,1]);
ptm$grid <- mean(c(mtm[1,2],mtm[1,1]));
ptmw$list <- 0.5 * (mtm[2,2] - mtm[2,1]);
ptm$list <- mean(c(mtm[2,2],mtm[2,1]));
ptmw$tabl <- 0.5 * (mtm[3,2] - mtm[3,1]);
ptm$tabl <- mean(c(mtm[3,2],mtm[3,1]));

pto <- list();
ptow <- list();
ptow$grid <- 0.5 * (mto[1,2] - mto[1,1]);
pto$grid <- mean(c(mto[1,2],mto[1,1]));
ptow$list <- 0.5 * (mto[2,2] - mto[2,1]);
pto$list <- mean(c(mto[2,2],mto[2,1]));
ptow$tabl <- 0.5 * (mto[3,2] - mto[3,1]);
pto$tabl <- mean(c(mto[3,2],mto[3,1]));

ptm;
sum(unlist(ptm));
ptmw;

pto;
sum(unlist(pto));
ptow;

rem <- list();
rem$grid <- c(pqw$grid / pq$grid, pdw$grid / pd$grid, ptmw$grid / ptm$grid);
rem$list <- c(pqw$list / pq$list, pdw$list / pd$list, ptmw$list / ptm$list);
rem$tabl <- c(pqw$tabl / pq$tabl, pdw$tabl / pd$tabl, ptmw$tabl / ptm$tabl);

rem;

pm <- list();
pm$grid <- prod(pq$grid, pd$grid, ptm$grid);
pm$list <- prod(pq$list, pd$list, ptm$list);
pm$tabl <- prod(pq$tabl, pd$tabl, ptm$tabl);

pm;

pmw <- list();
pmw$grid <- sum(rem$grid) * pm$grid;
pmw$list <- sum(rem$list) * pm$list;
pmw$tabl <- sum(rem$tabl) * pm$tabl;

pmw;


reo <- list();
reo$grid <- c(pqw$grid / pq$grid, pdw$grid / pd$grid, ptow$grid / pto$grid);
reo$list <- c(pqw$list / pq$list, pdw$list / pd$list, ptow$list / pto$list);
reo$tabl <- c(pqw$tabl / pq$tabl, pdw$tabl / pd$tabl, ptow$tabl / pto$tabl);

reo;

po <- list();
po$grid <- prod(pq$grid, pd$grid, pto$grid);
po$list <- prod(pq$list, pd$list, pto$list);
po$tabl <- prod(pq$tabl, pd$tabl, pto$tabl);

po;

pow <- list();
pow$grid <- sum(reo$grid) * po$grid;
pow$list <- sum(reo$list) * po$list;
pow$tabl <- sum(reo$tabl) * po$tabl;

pow;

c(sum(unlist(pm)), sum(unlist(pmw)));
c(sum(unlist(po)), sum(unlist(pow)));

