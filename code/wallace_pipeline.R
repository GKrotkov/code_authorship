#source("rcode2txt.R")
#source("load_rcode_data.R")
library(quanteda)
library(tidyverse)
library(glmnet)

# Load in namespace phrases and remove punctuation
load("../data/r_namespaces.rda")
namespaces <- str_replace_all(namespaces, "[[:punct:]]", "")

# Initial tokens chosen to be looked at for lasso
mw_all <- c(
  "space_placeholder",
  "assign_arrow_left_placeholder",
  "assign_arrow_right_placeholder",
  "global_assign_arrow_left_placeholder",
  "global_assign_arrow_right_placeholder",
  "pipe_arrow_right_placeholder",
  "pipe_arrow_left_placeholder",
  "matching_operators_placeholder",
  "matrix_multiplication_placeholder",
  "outer_product_placeholder",
  "magrittr_forward_pipe_placeholder",
  "magrittr_backward_pipe_placeholder",
  "kronecker_placeholder",
  "integer_division_placeholder",
  "default_value_placeholder",
  "modulo_placeholder",
  "namespace_placeholder",
  "triple_colon_placeholder",
  "comment_start_placeholder",
  "hyphen_placeholder",
  "plus_sign_placeholder",
  "asterisk_placeholder",
  "equal_sign_placeholder",
  "backslash_placeholder",
  "forwardslash_placeholder",
  "single_quote_placeholder",
  "double_quote_placeholder",
  "exclamation_point_placeholder",
  "dollar_sign_placeholder",
  "at_symbol_placeholder",
  "percent_symbol_placeholder",
  "carrot_symbol_placeholder",
  "and_symbol_placeholder",
  "open_parentheses_placeholder",
  "close_parentheses_placeholder",
  "open_brace_placeholder",
  "close_brace_placeholder",
  "open_bracket_placeholder",
  "close_bracket_placeholder",
  "colon_placeholder",
  "semicolon_placeholder",
  "question_mark_placeholder",
  "less_than_sign_placeholder",
  "greater_than_sign_placeholder",
  "grave_accent_placeholder",
  "approximate_placeholder",
  "period_placeholder",
  "comma_placeholder",
  "error",
  "else",
  "function",
  "return",
  "source",
  "param",
  "details",
  "null",
  "inf",
  "next",
  "for",
  "na",
  "library",
  "true",
  "false",
  "warning",
  "t",
  "f",
  "nan",
  "repeat",
  "break",
  "while",
  "integer",
  "complex",
  "real",
  "character",
  "ggplot",
  "plot",
  "summary",
  "lm",
  "gam",
  "glm",
  "unique",
  "geom",
  "table",
  "barplot",
  "hist",
  "histogram",
  "labs",
  "par",
  "mfrow",
  "theme",
  "predict",
  "omit",
  "head",
  "tail",
  "glimpse",
  "tibble",
  "merge",
  "str",
  "read",
  "csv",
  "write",
  "replace",
  "paste",
  "paste0"
)

# Attach namespace to mw_all and sort
mw_all <- c(mw_all,namespaces)
mw_all <- sort(mw_all)

# Empty list to store accuracy scores
accuracy_list <- c()

# Empty list to store PCA info
pca_info <- data.frame(author_id = character(0))

# Number of authors
author_num <- 7

accuracy_list <- c()

# Loop through all unique pairings of authors
for (i in 1:author_num) {
  for (j in i:author_num) {
    if (i != j) {
      print(c(i,j))
      # Get indices of authors' writings to be compared
      indices <- c((i*10-9):(i*10),(j*10-9):(j*10))
      
      # Filter the data to only have selectrd authors
      code_meta <- code_meta_df[indices,]
      code_txt <- code_txt_df[indices,]
      actual_authors <- actual_authors_df[indices,]
      
      # Store the author ids of both authors
      author1 <- actual_authors$author_id[1]
      author2 <- actual_authors$author_id[11]
      
      named_vector <- code_txt$document_id
      
      # Create tokens and dfm
      code_tokens <- code_txt %>%
        corpus() %>%
        tokens(remove_punct = TRUE, remove_symbols = TRUE, what = "word")
      
      code_tokens <- set_names(code_tokens, named_vector)
      
      code_dfm <- code_tokens %>%
        dfm() %>%
        dfm_weight(scheme = "prop")
      
      
      # Convert dfm to data frame using as.data.frame.matrix
      code_dfm_df <- as.data.frame.matrix(code_dfm)
      
      # Add the document_id column
      code_dfm_df$document_id <- rownames(code_dfm_df)
      
      code_dfm_df
      
      # Reorder columns
      code_dfm_df <- code_dfm_df %>%
        dplyr::select(document_id, names(sort(colMeans(.[,-ncol(code_dfm_df)]), decreasing = TRUE)))
      
      
      # Join dfm with code meta
      code_dfm_df <- code_dfm_df %>% 
        right_join(code_meta) %>% 
        dplyr::select(document_id, author_id, everything()) %>% 
        as_tibble()
      
      # define authors and disputed
      train_dfm <- code_dfm_df %>% dplyr::filter(author_id == author1 | author_id == author2)
      test_dfm <- code_dfm_df %>% dplyr::filter(author_id == "disputed")
      
      train_dfm
      
      # Define training and testing sets to only have selected tokens
      train_dfm <- train_dfm %>% 
        dplyr::select(document_id, author_id, one_of(mw_all)) %>%
        mutate(author_id = factor(author_id)) %>%
        column_to_rownames("document_id")
      
      test_dfm <- test_dfm %>% 
        dplyr::select(document_id, author_id, one_of(mw_all)) %>%
        column_to_rownames("document_id")
      
      train_dfm[is.na(train_dfm)] <- 0
      
      # Fit cv using glmnet
      cv_fit <- cv.glmnet(as.matrix(train_dfm[, -1]), train_dfm[, 1], 
                          family = "binomial")
      
      # Plot cv
      #plot(cv_fit)
      #title(main="Sample Lasso Optimization Between Two Authors", line = 2.5)
      
      # Get the model coeffictients as a dataframe
      coefs <- coef(cv_fit, s = "lambda.min") %>%
        as.matrix() %>%
        data.frame() %>%
        rownames_to_column("Variable") %>%
        dplyr::filter(s1 !=0) %>%
        dplyr::rename(Coeff = s1)
      
      # Remove the intercept
      coefs <- coefs[-1, , drop = FALSE]
      
      # Fit with lasso
      lasso_fit <- glmnet(as.matrix(train_dfm[, -1]), train_dfm[, 1], 
                          alpha = 1, family = "binomial", 
                          lambda = cv_fit$lambda.min)
      x_test <- model.matrix(author_id ~., test_dfm)[,-1]
      lasso_prob <- predict(cv_fit, newx = x_test, 
                            s = cv_fit$lambda.1se, type = "response")
      lasso_predict <- ifelse(lasso_prob > 0.5, author2, author1)
      
      # Data frame of authorship assignment
      results <- data.frame(lasso_predict, lasso_prob) %>% 
        dplyr::rename(pred_author_id = s1, prob = s1.1)

      # Make the index row a column
      results <- results %>%
        tibble::rownames_to_column(var = 'document_id')
      
      # Add a column with the actual authors
      results <- merge(results, actual_authors, by = "document_id")
      print(results)
      
      # Determine the accuracy of the model
      accuracy <- sum(results$pred_author_id == results$author_id) / 
        nrow(results)
      
      # Store the accuracy in a list
      accuracy_list <- c(accuracy_list, accuracy)

      # Make sure model is not empty
      if(nrow(coefs) != 0){
        # Store model coefficients as columns and add them to the PCA df
        new_vars <- pivot_wider(coefs, 
                                names_from = Variable, values_from = Coeff)
        new_vars$author_id <- paste(author1, author2)
        pca_info <- merge(pca_info, new_vars, all = TRUE)
      }
    }
  }
}

# Get rid of author id
rownames(pca_info) <- pca_info$author_id
pca_info$author_id <- NULL
pca_info[is.na(pca_info)] <- 0
# save the PCA dataframe
save(pca_info, file = "../data/pca_df.rda")
mean(accuracy_list)
