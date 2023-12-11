# Install packages
library(tidyverse)
library(tidytext)
library(SnowballC)
library(scales)

# Load in separate text files for sentiments to be compared
text1 <- paste(readLines("/PATH/file1.txt"), collapse = " ")
text2 <- paste(readLines("/PATH/file2.txt"), collapse = " ")
text3 <- paste(readLines("/PATH/file3.txt"), collapse = " ")

# Remove all punctuation from the text
text1 <- gsub("[[:punct:]]", " ", text1)
text2 <- gsub("[[:punct:]]", " ", text2)
text3 <- gsub("[[:punct:]]", " ", text3)

# Categorize each text and put them in a datafram
category <- c("cat1", "cat2", "cat3")
text <- c(text1, text2, text3)
df <- data.frame(category, text)

# Tokenize the texts by category
tidy_text_tokens <- df %>%
  unnest_tokens(category, text)

# Load in stop words and apply them
data(stop_words)
tidy_text_tokens <- tidy_text_tokens %>%
  dplyr::filter(!(word %in% stop_words$word))

# Simplify words to stems
tidy_text_tokens <- tidy_text_tokens %>%
  mutate(stem = wordStem(word))

# Use Bing sentiment lexicon to classify sentiments for tokens
tidy_sentiment_tokens <- tidy_text_tokens %>%
  inner_join(get_sentiments("bing"))

# Ensure labels are correct for ggplot
tidy_sentiment_tokens$category <- factor(tidy_sentiment_tokens$category,
                                         levels = c("cat1", "cat2", "cat3")
)

# Change sentiment labels to be capitalized
tidy_sentiment_tokens$sentiment <- gsub("negative", "Negative", tidy_sentiment_tokens$sentiment)
tidy_sentiment_tokens$sentiment <- gsub("positive", "Positive", tidy_sentiment_tokens$sentiment)

# Visualize the sentiments
tidy_sentiment_tokens %>%
  group_by(category, sentiment) %>%
  summarize(n_words = n()) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(total_assigned_words = sum(n_words)) %>%
  ungroup() %>%
  ggplot(aes(
    x = category, y = n_words,
    fill = sentiment
  )) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#FF8888", "#4A5DFF")) +
  theme_bw() +
  geom_hline(aes(yintercept = 0.5), size = 1.5, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Sentiment Analysis Conducted ...",
    y = "Sentiment %",
    x = "Category",
    fill = "Sentiment"
  )