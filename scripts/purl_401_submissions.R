source("R/purler.R")

# 401/402 data analysis structure is assumed ()

sensitive_ids <- unique(sapply(strsplit(filenames, "_"), function(s) s[1]))
new_ids <- id_gen(length(sensitive_ids))
new_ids <- rep(new_ids, each = 2)
new_ids <- paste0(new_ids, c("_1", "_2"))
target_paths <- paste(output_dir, "/", new_ids, ".R", sep = "")

purl_filepaths_to_R(filepaths, output_dir)