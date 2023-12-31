# purler.R
# goal: purl() out r code from student submissions, save it in a file with a 
# coded name (unique and consistent, but not the andrewID input)
library(knitr)

#' Generate Random IDs
#' 
#' Generates a given number of random, but consistent, id values. Will always
#' have exactly 10 characters - 5 random letters, 4 random numbers, and 1 random
#' trailing letter.
#' @param n Number of ids to generate.
id_gen <- function(n = 1) {
    return(paste0(
        do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE)), 
        sprintf("%04d", sample(1000:9999, n, TRUE)), 
        sample(LETTERS, n, TRUE)))
}

#' Purl Filepaths
#' 
#' Purls R code out from RMDs given a vector of filepaths
#' @param filepaths vector of filepaths to .Rmd files
#' @param output_dir directory to output .R code to
purl_filepaths <- function(filepaths, output_dir){
    target_paths <- paste0(output_dir, "/file", 1:length(filepaths), ".R")
    mapply(knitr::purl, filepaths, target_paths)
}

#' Purl Directory
#' 
#' Purls R code out from all .Rmd files in a given directory
#' @param input_dir Directory with .Rmd files
#' @param output_dir Target directory for .R outputs
purl_dir <- function(input_dir, output_dir){
    filenames <- list.files(input_dir, pattern = "*.Rmd$")
    filepaths <- paste(input_dir, filenames, sep = "/")
    purl_filepaths(filepaths, output_dir)
}
