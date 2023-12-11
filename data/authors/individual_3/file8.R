## ----global options, include = FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)


## --------------------------------------------------------------------------------------------------------
library(tidyr)


## --------------------------------------------------------------------------------------------------------
library(countrycode)
library(tidyverse)
library(ggnewscale)
library(ggplot2)
library(ggseas)
library(geomtextpath)
futbol = read_csv2("./2021-2022 Football Player Stats.csv")
extra_match = c("Africa", "Africa", "Europe", "Africa", "Africa", "Americas", "Americas", "Europe", "Africa", "Europe", "Europe", "Africa", "Africa", "Europe", "Europe", "Americas", "Africa", "Americas", "Europe", "Africa", "Europe", "Europe", "Americas", "Asia", "Europe", "Africa", "Europe", "Europe", "Africa", "Americas", "Europe", "Africa", "Africa")
names(extra_match) = c("ALG", "ANG", "BUL", "CGO", "CHA", "CHI", "CRC", "CRO", "CTA", "DEN", "ENG", "EQG", "GAM", "GER", "GRE", "GRN", "GUI", "HON", "KVX", "MAD", "NED", "NIR", "PAR", "PHI", "POR", "RSA", "SCO", "SUI", "TOG", "URU", "WAL", "ZAM", "ZIM")

extra_match2 = c("Algeria", "Angola", "Bulgaria", "Congo", "Chad", "Chile", "Costa Rica", "Croatia", "Central African Republic", "Denmark", "England", "Equatorial Guinea", "Gambia", "Germany", "Greece", "Grenada", "Guinea", "Honduras", "Kosovo", "Madagascar", "Netherlands", "Northern Ireland", "Paraguay", "Philippines", "Portugal", "South Africa", "Scotland", "Switzerland", "Togo", "Uruguay", "Wales", "Zambia", "Zimbabwe")
names(extra_match2) = c("ALG", "ANG", "BUL", "CGO", "CHA", "CHI", "CRC", "CRO", "CTA", "DEN", "ENG", "EQG", "GAM", "GER", "GRE", "GRN", "GUI", "HON", "KVX", "MAD", "NED", "NIR", "PAR", "PHI", "POR", "RSA", "SCO", "SUI", "TOG", "URU", "WAL", "ZAM", "ZIM")
futbol$Continent = countrycode(futbol$Nation, "iso3c", "continent", custom_match = extra_match)


#futbol$Country = countrycode(futbol$Nation, "iso3c", "country.name", custom_match = extra_match2)
simpPos = function (x) {
  if (x == "MFFW") {
    return ("MF")
  } else if (x == "FWMF") {
    return ("FW")
  } else if (x == "DFMF") {
    return ("DF")
  } else if (x == "FWDF") {
    return ("FW")
  } else if (x == "MFDF") {
    return ("MF")
  } else if (x == "DFFW") {
    return ("DF")
  } else if (x == "GKMF") {
    return ("GK")
  } else {
    return (x)
  }
}
futbol$ShoDist = futbol$ShoDist/10
futbol$Pos_simplified = sapply(futbol$Pos, FUN = simpPos)

world = map_data("world")



## --------------------------------------------------------------------------------------------------------

futbol$Goals = as.numeric(futbol$Goals)
futbol$`G/SoT` = as.numeric(futbol$`G/SoT`)
futbol$Shots = as.numeric(futbol$Shots)


noGoalie = filter(futbol, Pos_simplified=="DF" | Pos_simplified=="FW" | Pos_simplified == "MF")
noGoalieorZero = filter(noGoalie, ShoDist!=0)


meanDF = median(filter(futbol, Pos_simplified== "DF")$ShoDist)
meanFW = median(filter(futbol, Pos_simplified== "FW")$ShoDist)
meanMF = median(filter(futbol, Pos_simplified== "MF")$ShoDist)
meanGK = median(filter(futbol, Pos_simplified== "GK")$ShoDist)

lines = c(6, 18.5, 57.5)
groupcolors = c("salmon", "turquoise", "mediumorchid1")
noGoalieorZero %>% ggplot(aes(x = ShoDist)) +
geom_density(aes(fill = Pos_simplified), alpha = 0.5, adjust = 1) +
  geom_textvline(xintercept = 6, label = "goal line", hjust = 0.8,
               linetype = "dashed", vjust = 1.3, color = "black") +
  geom_textvline(xintercept = 18.5, label = "penalty line", hjust = 0.8,
               linetype = "dashed", vjust = 1.3, color = "black") +
  geom_textvline(xintercept = 57.5, label = "center line", hjust = 0.8,
               linetype = "dashed", vjust = 1.3, color = "black") +
  scale_fill_discrete(name = "Position", labels = c("Defender", "Forward", "Midfielder")) + 
  labs(title = "Distribution of Shot Distance across Player Position", x = "Shot Distance (Yards)", y = "Density")  



## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------
library(tidyverse)
data <- read.csv("https://raw.githubusercontent.com/FSKoerner/Fall23-36315-data/main/redditData.csv")


## ---- message = FALSE, warning = FALSE-------------------------------------------------------------------
library(tm)
title <- Corpus(VectorSource(data$title))


## --------------------------------------------------------------------------------------------------------
#change to lowercase
title <- tm_map(title, content_transformer(tolower))
# Remove punctuations
title <- tm_map(title, removePunctuation)
# Eliminate extra white spaces
title <- tm_map(title, stripWhitespace)


## --------------------------------------------------------------------------------------------------------
#make document term matrix
DocumentTermMatrix(title)


## --------------------------------------------------------------------------------------------------------
dtm.title <- DocumentTermMatrix(title, control = list(stopwords = TRUE, stemming = TRUE))
dtm.title


## --------------------------------------------------------------------------------------------------------
library(forcats)
library(tidytext)

dtm.tidy <- tidy(dtm.title)

term.count <- dtm.tidy %>%
  group_by(term) %>%
  summarize(n.total=sum(count)) %>%
  arrange(desc(n.total))
head(term.count)


## --------------------------------------------------------------------------------------------------------
data.female = filter(data, gender=="F")
data.male = filter(data,gender=="M")
title.female <- Corpus(VectorSource(data.female$title))
title.male <- Corpus(VectorSource(data.male$title))



## --------------------------------------------------------------------------------------------------------
title.female <- tm_map(title.female, content_transformer(tolower))
title.female <- tm_map(title.female, removePunctuation)
title.female <- tm_map(title.female, stripWhitespace)

title.male <- tm_map(title.male, content_transformer(tolower))
title.male <- tm_map(title.male, removePunctuation)
title.male <- tm_map(title.male, stripWhitespace)

dtm.title.female <- DocumentTermMatrix(title.female, control = list(stopwords = TRUE, stemming = TRUE))
dtm.title.male <- DocumentTermMatrix(title.male, control = list(stopwords = TRUE, stemming = TRUE))


## --------------------------------------------------------------------------------------------------------
 dtm.title.male
 dtm.title.female


## --------------------------------------------------------------------------------------------------------
dtm.tidy.female <- tidy(dtm.title.female)
dtm.tidy.male <- tidy(dtm.title.male)

female.term.count <- dtm.tidy.female %>%
  group_by(term) %>%
  summarize(n.total=sum(count)) %>%
  arrange(desc(n.total))
head(female.term.count, 10)

male.term.count <- dtm.tidy.male %>%
  group_by(term) %>%
  summarize(n.total=sum(count)) %>%
  arrange(desc(n.total))
head(male.term.count, 10)


## --------------------------------------------------------------------------------------------------------
library(wordcloud)
dtm.title
title.matrix <- as.matrix(dtm.title)
words <- sort(rowSums(title.matrix),decreasing=TRUE)
words
df <- data.frame(word = names(words),freq=words)

wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND WORD CLOUDS HERE


## --------------------------------------------------------------------------------------------------------
#put the male and female titles into one corpus
# titleMaleFemale <- tm:::c.VCorpus(title.male, title.female)
# titleMaleFemale <- tm_map(titleMaleFemale, PlainTextDocument)

#remove stop words, perform stemming
# titleMaleFemale <- tm_map(titleMaleFemale, removeWords, stopwords("english"))
# titleMaleFemale <- tm_map(titleMaleFemale, stemDocument)

#term-document matrix
# tdm_maleFemale <- TermDocumentMatrix(titleMaleFemale)

#convert to a matrix class
# tdm_maleFemale <- as.matrix(tdm_maleFemale)


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND COMPARISON WORD CLOUD HERE


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND COMPARISON WORD CLOUD HERE


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE HERE


## --------------------------------------------------------------------------------------------------------
library(tidytext)
# PUT YOUR CODE HERE


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE HERE


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE HERE


## --------------------------------------------------------------------------------------------------------
# PUT YOUR CODE AND WORD CLOUDS HERE


## --------------------------------------------------------------------------------------------------------
# library(topicmodels)
# library(tidytext)
# library(ggplot2)
# library(tidyverse)
# #run Latent Dirichlet Allocation
# title_lda <- LDA(dtm.title, k = 2, control = list(seed = 1234))
# #grab the word-topic probabilities
# title_topics <- tidy(title_lda, matrix = "beta")
# #grab the words with the top ten probabilities (betas),
# #and then organize the data by topic, decreasing by beta
# title_top_terms <- title_topics %>%
#   group_by(topic) %>%
#   top_n(10, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
# #make the plot of betas for each topic
# title_top_terms %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip() +
#   scale_x_reordered()

