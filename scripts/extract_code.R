# extract_code.r
# goal: purl() out r code from student submissions, save it in a file with a
# coded name (unique and consistent, but not the andrewID input)
library(knitr)

# id_gen from Alex Reinhart
id_gen <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

input_dir <- "data/401-code"
output_dir <- "data/401-anonymized"

filenames <- list.files(input_dir, pattern = "*.Rmd$")

filepaths <- file.path(input_dir, filenames)

file_authors <- sapply(strsplit(filenames, "_"), function(s) s[1])
sensitive_ids <- unique(file_authors)
new_ids <- id_gen(length(sensitive_ids))
names(new_ids) <- sensitive_ids
# attach an increasing number to the end of each file to ensure uniqueness
target_names <- paste0(new_ids[file_authors], 1:length(file_authors), 
                       ".R")
target_paths <- file.path(output_dir, target_names)

# check length conditions
stopifnot(length(filepaths) == length(target_paths))
stopifnot(length(target_paths) == length(unique(target_paths)))

for (i in seq_along(filepaths)) {
  knitr::purl(filepaths[i],
              output = target_paths[i])
}
