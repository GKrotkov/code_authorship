# purler.R
# goal: purl() out r code from student submissions, save it in a file with a 
# coded name (unique and consistent, but not the andrewID input)
library(knitr)

# id_gen from Alex Reinhart
id_gen <- function(n = 1) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

# Change these inputs!
input_dir <- "data/inputs/nphillip"
output_dir <- "data/outputs"

filenames <- list.files(input_dir)
filepaths <- paste(input_dir, filenames, sep = "/")

purl_filepaths_to_R <- function(filepaths, output_dir, subfolder = ""){
    target_path <- paste(output_dir, subfolder, sep = "/")
    target_paths <- paste0(target_path, "/file", 1:length(filepaths), ".R")
    mapply(knitr::purl, filepaths, target_paths)
}

# code that assumes the 401/402 data analysis exam structure
#sensitive_ids <- unique(sapply(strsplit(filenames, "_"), function(s) s[1]))
#new_ids <- id_gen(length(sensitive_ids))
#new_ids <- rep(new_ids, each = 2)
#new_ids <- paste0(new_ids, c("_1", "_2"))
#target_paths <- paste(output_dir, "/", new_ids, ".R", sep = "")

#purl_filepaths_to_R(filepaths, output_dir)