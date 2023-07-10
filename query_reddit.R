## Install RedditExtractoR package first;
install.packages("RedditExtractoR")

## then run this file line by line
library(tidyverse)
library(RedditExtractoR)
source("https://raw.githubusercontent.com/multidis/hult-inter-bus-reports-r/main/text_mining/query_reddit_funs.R")

# set your chosen subreddit string here
subr <- "books"

# limit the number of threads to query (be careful not to set too large)
nthreads <- 50

# change "week" to one of (hour, day, week, month, year) depending
#  on your chosen subreddit activity
# CAUTION: avoid too long time periods as your computer may be blocked
subr_urls <- find_thread_urls(subreddit=subr, sort_by="top", period="week")
str(subr_urls)
head(subr_urls)
tail(subr_urls)

# sort by the number of comments
subr_urls_sort <- subr_urls %>% arrange(desc(comments))

# collect all threads info (main call)
threads_df <- collect_all_threads(subr_urls_sort$url, nthreads)
str(threads_df)

save(threads_df, file = paste0(subr, "books", nthreads, "threads.rda"))
