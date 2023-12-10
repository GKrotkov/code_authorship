library(tidyverse)
library(roxygen2)
pkgs <- c("base", "utils", "stats", "ggplot2", "dplyr", "tibble", "readr", 
          "roxygen2")
namespaces <- c()

get_namespace <- function(ns){
    return(ls(name = asNamespace(ns), all.names = TRUE))
}

for (i in 1:length(pkgs)){
    namespaces <- c(namespaces, get_namespace(pkgs[i]))
}

save(namespaces, file = "r_namespaces.rda")
