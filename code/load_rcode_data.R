#source("rcode2txt.R")

path <- "/Users/zachstrennen/Downloads/kraken.R"
gabe_code_1 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/tba_helpR.r"
gabe_code_2 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/tba_interfaceR.r"
gabe_code_3 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/tba_readR.r"
gabe_code_4 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/tba_tidyR.r"
gabe_code_5 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/file1.R"
gabe_code_6 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/file2.R"
gabe_code_7 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/file3.R"
gabe_code_8 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/file4.R"
gabe_code_9 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/file5.R"
gabe_code_10 <- reformat_text_R(path, replacements)

path <- "~/hw4app/starwars2.R"
zach_code_1 <- reformat_text_R(path, replacements)

path <- "~/hw4app/app.R"
zach_code_2 <- reformat_text_R(path, replacements)

path <- "~/hw4app/starwars2model.R"
zach_code_3 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/Sentiment-Analysis-Comp-Viz.R"
zach_code_4 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/Vinyl Collection Visualizations.R"
zach_code_5 <- reformat_text_R(path, replacements)

path <- "~/hw4app/linkedin_sector_conversion.R"
zach_code_6 <- reformat_text_R(path, replacements)

path <- "~/hw4app/lin_mod_midterm.R"
zach_code_7 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/final visualizations res life.R"
zach_code_8 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/majorminor.R"
zach_code_9 <- reformat_text_R(path, replacements)

path <- "/Users/zachstrennen/Downloads/census initial analysis.R"
zach_code_10 <- reformat_text_R(path, replacements)

document_id <- c("text1","text2","text3","text4","text5","text6","text7","text8","text9","text10",
                 "text11","text12","text13","text14","text15","text16","text17","text18","text19","text20")

author_id <- c(rep("gabe",10), rep("zach",10))

actual_authors_df <- data.frame(document_id, author_id)

gabe_indices <- sample(which(author_id == "gabe"), 3)

zach_indices <- sample(which(author_id == "zach"), 3)

author_id[gabe_indices] <- "disputed"
author_id[zach_indices] <- "disputed"

code_meta_df <- data.frame(document_id, author_id)

text <- c(gabe_code_1,gabe_code_2,gabe_code_3,gabe_code_4,gabe_code_5,
          gabe_code_6,gabe_code_7,gabe_code_8,gabe_code_9,gabe_code_10,
          zach_code_1,zach_code_2,zach_code_3,zach_code_4,zach_code_5,
          zach_code_6,zach_code_7,zach_code_8,zach_code_9,zach_code_10)

code_txt_df <- data.frame(document_id, text)

