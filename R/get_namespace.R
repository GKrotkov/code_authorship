library(tidyverse)
library(roxygen2)
library(devtools)

get_namespace <- function(ns){
    return(ls(name = asNamespace(ns), all.names = TRUE))
}


get_namespaces <- function(pkgs = c("base", "utils", "stats", "ggplot2", 
                                    "dplyr", "tibble", "readr", "roxygen2", 
                                    "devtools"), 
                           tofile = "data/r_namespaces.rda"){
    namespaces <- c()
    
    for (i in 1:length(pkgs)){
        namespaces <- c(namespaces, get_namespace(pkgs[i]))
    }
    
    save(namespaces, file = tofile)
}