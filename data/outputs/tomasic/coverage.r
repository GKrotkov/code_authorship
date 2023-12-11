pq <- list();
pq$table = 0.95;
pq$list = 0.60;
pq$grid = 0.68;

pd <- list();
pd$table = 0.85;
pd$list = 0.81;
pd$grid = 0.81;

pqd <- list();
pqd$table = pq$table * pd$table;
pqd$list = pq$list * pd$list;
pqd$grid = pq$grid * pd$grid;

pqd;

ptm <- list();
ptm$table = 0.37;
ptm$list = 0.48;
ptm$grid = 0.14;
ptm$n = 351;

pto <- list();
pto$table = 0.16;
pto$list = 0.52;
pto$grid = 0.32;
pto$n = 104;

pqm <- list();
pqm$table = ptm$table * pqd$table;
pqm$list = ptm$list * pqd$list;
pqm$grid = ptm$grid * pqd$grid;

pqo <- list();
pqo$table = pto$table * pqd$table;
pqo$list = pto$list * pqd$list;
pqo$grid = pto$grid * pqd$grid;

pqm;
pqo;

sum(unlist(pqm));
sum(unlist(pqo));

pto$var = cbind(c(0,0,0),c(0,0,0),c(0,0,0));
pto$var[1,1] = pto$n * pto$table * (1.0-pto$table);
pto$var[2,2] = pto$n * pto$list * (1.0-pto$list);
pto$var[3,3] = pto$n * pto$grid * (1.0-pto$grid);
pto$var[1,2] = pto$var[2,1] = -1.0 * pto$n * pto$table * pto$list;
pto$var[1,3] = pto$var[3,1] = -1.0 * pto$n * pto$table * pto$grid;
pto$var[2,3] = pto$var[3,2] = -1.0 * pto$n * pto$list * pto$grid;

pto$var;

ptm$var = cbind(c(0,0,0),c(0,0,0),c(0,0,0));
ptm$var[1,1] = ptm$n * ptm$table * (1.0-ptm$table);
ptm$var[2,2] = ptm$n * ptm$list * (1.0-ptm$list);
ptm$var[3,3] = ptm$n * ptm$grid * (1.0-ptm$grid);
ptm$var[1,2] = ptm$var[2,1] = -1.0 * ptm$n * ptm$table * ptm$list;
ptm$var[1,3] = ptm$var[3,1] = -1.0 * ptm$n * ptm$table * ptm$grid;
ptm$var[2,3] = ptm$var[3,2] = -1.0 * ptm$n * ptm$list * ptm$grid;

ptm$var;

