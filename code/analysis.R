# Data Wrangling and Visualization
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
# Date & Time Manipulation.
library(hms)
library(lubridate) 
# Text Mining
library(tidytext)
library(tm)
library(wordcloud)
# Network Analysis
library(igraph)
# Network Visualization (D3.js)
library(networkD3)

#-------------------------------------------------------------------
# READING

# Set notebook directory.
dir <- "C:/Users/User/Desktop/GitHub Projects/TwitterPeaceCol"

# Set file path.
file.name <- paste0(dir,'/data/plebiscito.json')

# Read each line. 
tweets.raw.list <- map(
  .x = read_lines(file = file.name), 
  .f = rjson::fromJSON
)


# Parse subset of the data into a tibble. 
tweets.raw.df <- tweets.raw.list %>% 
  map_df(.f = ~ data.frame(
    # Select non-user related data.
    ID = .x$id_str,
    Created_At = .x$created_at,
    Text = .x$text, 
    stringsAsFactors = FALSE
  )
  ) %>% 
  as_tibble()

tweets.raw.df %>% 
  # We  do not want to display accounts.
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()

tweets.raw.df %>% glimpse()

tweets.raw.df %>% 
  slice(1:4) %>% 
  pull(Created_At) 


tweets.raw.df %<>% 
  mutate(
    Created_At = Created_At %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date. 
      parse_date_time(orders = '%a %b %d %H%M%S %Y',locale = "English" )
  )

tweets.raw.df %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()


#-------------------------------------------------------------------
# Temporal Analysis

# We substract seconds, that is why we need three factors. 
tweets.raw.df %<>% 
  mutate(Created_At = Created_At - 5*60*60)

tweets.raw.df %>% pull(Created_At) %>% min()

tweets.raw.df %>% pull(Created_At) %>% max()

tweets.raw.df %<>% 
  mutate(Created_At_Round = Created_At %>% round(units = 'mins') %>% as.POSIXct())


# Plot of the time series count per minute

plt <- tweets.raw.df %>% 
  count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Minute')

plt %>% ggplotly()


results.time <- as.POSIXct(x = '2016-10-02 19:28:00')

tweets.raw.df %>% 
  filter(Created_At_Round > results.time ) %>% 
  select(Text) %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  pull(Text) %>% 
  head(20) 


#-------------------------------------------------------------------
# Preprocessing

tweets.df <- tweets.raw.df %>% 
  # Remove column.
  select(-  Created_At) %>% 
  # Convert to lowercase. 
  mutate(Text = Text %>% str_to_lower) %>% 
  # Remove unwanted characters. 
  mutate(Text= Text %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = '&amp')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'https')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags.
  mutate(Text = Text %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts.
  mutate(Text = Text %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweets.
  mutate(Text = Text %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(Text = Text %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(Text = Text %>% str_remove_all(pattern = '\\_')) 

# Replace accents. 
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')

tweets.df %<>% 
  mutate(Text = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                       new = replacement.list %>% str_c(collapse = ''),
                       x = Text))

# Corpus object

corpus <-  Corpus(x = VectorSource(x = tweets.df$Text))

tweets.text <- corpus %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('spanish')) %>% 
  tm_map(PlainTextDocument) # %>% 
# We could also use stemming by uncommenting the folowing line. 
# tm_map(stemDocument, 'spanish')

# Recover data into original tibble.
tweets.df %<>% mutate(Text = tweets.text[[1]]$content)

GetHashtags <- function(tweet) {
  
  hashtag.vector <- str_extract_all(string = tweet, pattern = '#\\S+', simplify = TRUE) %>% 
    as.character()
  
  hashtag.string <- NA
  
  if (length(hashtag.vector) > 0) {
    
    hashtag.string <- hashtag.vector %>% str_c(collapse = ', ')
    
  } 
  
  return(hashtag.string)
}

hashtags.df <- tibble(
  Hashtags = tweets.raw.df$Text %>% map_chr(.f = ~ GetHashtags(tweet = .x))
)

hashtags.df %>% head()

tweets.df %<>% bind_cols(hashtags.df) 

# "m" will represent before. results.time. 
tweets.m.df <- tweets.df %>% 
  filter(Created_At_Round < results.time) %>% 
  select(ID, Text)


# "p" will represent after results.time. 
tweets.p.df <- tweets.df %>% 
  filter(Created_At_Round >= results.time) %>% 
  select(ID, Text)

#-------------------------------------------------------------------
# Counts