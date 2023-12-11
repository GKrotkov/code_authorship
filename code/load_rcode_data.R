#source("rcode2txt.R")

data_path <- "../data"
authors <- c("group_1", "group_2", "group_3", "group_4",
             "individual_1", "individual_2", "individual_3", "individual_4")
author_id <- rep(authors, each = 10)
author_paths <- paste(data_path, "authors", authors, sep = "/")
filenames <- c(mapply(list.files, author_paths))
# ASSUMPTION: there are exactly 10 R files in each directory
names(filenames) <- author_id
filepaths <- paste(rep(author_paths, each = 10), filenames, sep = "/")
names(filepaths) <- author_id

text <- mapply(reformat_text_R, filepaths, MoreArgs = list(replacements))

document_id <- paste("text", 1:length(text), sep = "")

test_size <- 4

actual_authors_df <- data.frame(document_id, author_id)

withhold_idx <- c(sample(which(author_id == "group_1"), test_size), 
                  sample(which(author_id == "group_2"), test_size), 
                  sample(which(author_id == "group_3"), test_size), 
                  sample(which(author_id == "group_4"), test_size),
                  sample(which(author_id == "individual_1"), test_size), 
                  sample(which(author_id == "individual_2"), test_size), 
                  sample(which(author_id == "individual_3"), test_size), 
                  sample(which(author_id == "individual_4"), test_size))


author_id[withhold_idx] <- "disputed"

code_meta_df <- data.frame(document_id, author_id)

code_txt_df <- data.frame(document_id, text)

