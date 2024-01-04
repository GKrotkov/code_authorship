library(tidyverse)
library(roxygen2)
library(devtools)

#' Get Namespace
#' 
#' Given a package title, returns the entire namespace of that package
#' @param pkg string title 
get_namespace <- function(pkg){
    return(ls(name = asNamespace(pkg), all.names = TRUE))
}


#' Get Namespaces
#' 
#' Gets the namespace for a set of packages
#' @param pkgs vector of packages
#' @param tofile target file for namespaces vector output
get_namespaces <- function(pkgs = c("base", "utils", "stats", "ggplot2", 
                                    "dplyr", "tibble", "readr", "roxygen2", 
                                    "devtools"), 
                           tofile = "data/r_namespaces.rda"){
    namespaces <- c()
    
    for (i in seq_along(pkgs)){
        namespaces <- c(namespaces, get_namespace(pkgs[i]))
    }
    
    save(namespaces, file = tofile)
}
