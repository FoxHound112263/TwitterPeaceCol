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
      parse_date_time(orders = '%a %b %d %H%M%S %Y')
  )

tweets.raw.df %>% 
  filter(!str_detect(string = Text, pattern = '@')) %>% 
  head()
