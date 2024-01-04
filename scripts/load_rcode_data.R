library(devtools)
load_all()

data_path <- "data"

# ASSUMPTION: there are exactly 10 R files in each directory
authors <- list.files(paste(data_path, "authors", sep = "/"))
author_id <- rep(authors, each = 10)

# get all filenames
author_paths <- list.dirs(paste(data_path, "authors", sep = "/"), 
                          recursive = FALSE)
filenames <- c(mapply(list.files, author_paths))
names(filenames) <- author_id

# get all filepaths
filepaths <- paste(rep(author_paths, each = 10), filenames, sep = "/")
names(filepaths) <- author_id

text <- mapply(reformat_text_R, filepaths, MoreArgs = list(replacements))

document_id <- paste("text", 1:length(text), sep = "")

test_size <- 4 # treated as a hyperparameter, tuned to minimize CV error

actual_authors_df <- data.frame(document_id, author_id)

withhold_idx <- c()
for (author in authors){
    withhold_idx <- c(withhold_idx, 
                      sample(which(author_id == author), test_size))
}

author_id[withhold_idx] <- "disputed"

code_meta_df <- data.frame(document_id, author_id)

code_txt_df <- data.frame(document_id, text)
