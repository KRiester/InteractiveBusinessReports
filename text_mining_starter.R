## Save Reddit dataset first, to be loaded here.
## Install tidytext, wordcloud packages.
library(tidyverse)
library(tidytext)
library(wordcloud)

# previously saved Reddit dataset;
# here we use an example dataset from
# https://github.com/multidis/hult-inter-bus-reports-r/tree/main/text_mining/datasets_reddit
load(file = "booksbooks50threads.rda")
str(threads_df)

# tidy table: text column to unite thread's title, text, and comments
threads_tbl <- as_tibble(threads_df) %>%
  unite(title, text, text_comments, col = "text", sep = " ")
threads_tbl
threads_tbl$text[1]

# tokenization:
# unnest_tokens removes punctuation, converts text to lower case
threads_words <- threads_tbl %>%
  unnest_tokens(word, text) %>%
  # omit most rare words: keep those occurring more than 10 times
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()
threads_words

# remove stop words (corpus available within tidytext)
head(stop_words$word)
threads_words_clean <- threads_words %>%
  filter(!word %in% stop_words$word) %>%
  filter(!is.na(word))
threads_words_clean

# term frequency (tf)
threads_words_count <- threads_words_clean %>% count(word, sort = TRUE)
threads_words_count

# plot terms with frequencies exceeding a threshold
nmin <- 500
threads_words_count %>%
  filter(n > nmin) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE) +
  xlab(NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(title="Word frequency", subtitle=paste("n >", nmin)) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# most frequent terms across all threads
wordcloud(threads_words_count$word, threads_words_count$n, max.words = 30,
          colors=brewer.pal(3, "Dark2"), scale=c(2,1))

# tf-idf
# https://www.tidytextmining.com/tfidf.html
# NOTE: url plays a role of a unique thread identifier for idf computation
threads_words_tf_idf <- threads_words_clean %>%
  count(url, word, sort = TRUE) %>%
  bind_tf_idf(word, url, n) %>%
  group_by(word) %>% 
  summarise(tf_idf_sum = sum(tf_idf)) %>%
  arrange(desc(tf_idf_sum))
threads_words_tf_idf

# highest tf-idf terms across all threads
threads_words_tf_idf %>%
  top_n(15, tf_idf_sum) %>% 
  ggplot(aes(reorder(word, tf_idf_sum), tf_idf_sum)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(title="Term frequency and inverse document frequency (tf-idf)", 
       subtitle="Top words overall",
       x= NULL, 
       y= "tf-idf") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# if-idf based word cloud
wordcloud(threads_words_tf_idf$word, 
          threads_words_tf_idf$tf_idf_sum, 
          max.words = 50,
          colors=brewer.pal(3, "Dark2"), 
          scale=c(1, 2))

# ngram-analysis: filtering out stopwords
threads_bigram <- threads_tbl %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1)) %>%
  count(word1, word2, sort = TRUE)
threads_bigram

threads_bigram %>%
  top_n(15, n) %>% 
  ggplot(aes(reorder(paste(word1, word2), n), n)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(title="Bigram count", 
       subtitle="Total counts in all documents",
       x= NULL, 
       y= "count") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# trigrams
threads_trigram <- threads_tbl %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,  
         !is.na(word1)) %>%
  count(word1, word2, word3, sort = TRUE)
threads_trigram

threads_trigram %>%
  top_n(15, n) %>% 
  ggplot(aes(reorder(paste(word1, word2, word3), n), n)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(title="Trigram count", 
       subtitle="Total counts in all documents",
       x= NULL, 
       y= "count") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Create the dataset
year <- c(2018, 2019, 2020, 2021, 2022)
sales <- c(100090, 101380, 106780, 88530, 79450)

book_sales <- data.frame(Year = year, Sales = sales)

# Print the dataset
print(book_sales)

# Define color palette for each year
color_palette <- c("#845533", "#a8815e", "#b98b69", "#d6b396", "#d5cdc0")

# Visualize the data as a bar chart with different colors for each year
library(ggplot2)

library(plotly)

p1 <- ggplot(book_sales, aes(x = Year, y = Sales, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(title = "Book Sales in Fresh Minds Press (2018-2022)",
       x = "Year",
       y = "Number of Books Sold") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggplotly(p1)


View(threads_bigram)
