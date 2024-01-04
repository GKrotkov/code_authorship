# purler.R
# goal: purl() out r code from student submissions, save it in a file with a 
# coded name (unique and consistent, but not the andrewID input)
library(knitr)

# Generate random noise for output names
id_gen <- function(n = 1) {
    return(paste0(
        do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE)), 
        sprintf("%04d", sample(1000:9999, n, TRUE)), 
        sample(LETTERS, n, TRUE)))
}

purl_filepaths <- function(filepaths, output_dir){
    target_paths <- paste0(output_dir, "/file", 1:length(filepaths), ".R")
    mapply(knitr::purl, filepaths, target_paths)
}

purl_dir <- function(input_dir, output_dir){
    filenames <- list.files(input_dir, pattern = "*.Rmd$")
    filepaths <- paste(input_dir, filenames, sep = "/")
    purl_filepaths(filepaths, output_dir)
}
