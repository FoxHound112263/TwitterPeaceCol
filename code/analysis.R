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

# Remove the shortcut 'q' for 'que'.
extra.stop.words <- c('q')

stopwords.df <- tibble(
  word = c(stopwords(kind = 'es'), 
           # We have some tweets in english.
           stopwords(kind = 'en'),  
           extra.stop.words)
)

words.df <- tweets.df %>% 
  unnest_tokens(input = Text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

word.count <- words.df %>% count(word, sort = TRUE)

word.count %>% head(10)

word.count <-  word.count %>% mutate(word = reorder(word,n))

# Lollipop chart

ggplot(word.count[1:10,], aes(n,word,label=n,color=word)) +
  geom_segment(aes(x = 0, y = word, xend = n, yend = word), color = "grey50") +
  geom_point(size=3) +
  labs(title="Lollipop Chart",subtitle="Most frequent words in the tweets") +
  #coord_flip() +
  theme_minimal() +
  geom_text(position = position_nudge(x = 500)) +
  theme(panel.grid = element_blank(),legend.position = "none")

# Wordcloud

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

wordcloud2(data=word.count, size=1,shape = "oval",
           rotateRatio = 0.5, 
           ellipticity = 0.9, color = "brown")


# Split data

# Before results. 
words.m.df <- tweets.m.df %>% 
  unnest_tokens(input = Text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

word.count.m <- words.m.df %>% count(word, sort = TRUE)

plt.m <- word.count.m %>% 
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'blue', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'Before Results')

# After results. 
words.p.df <- tweets.p.df %>% 
  unnest_tokens(input = Text, output = word) %>% 
  anti_join(y = stopwords.df, by = 'word')

word.count.p <- words.p.df %>% count(word, sort = TRUE)

plt.p <- word.count.p %>% 
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  theme_light() + 
  geom_col(fill = 'red', alpha = 0.8) +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = 'After Results')

plot_grid(... = plt.m, plt.p)


#-------------------------------------------------------------------
# Hashtags

hashtags.unnested.df <- tweets.df %>% 
  select(Created_At_Round, Hashtags) %>% 
  unnest_tokens(input = Hashtags, output = hashtag)

hashtags.unnested.count <- hashtags.unnested.df %>% 
  count(hashtag) %>% 
  drop_na()


wordcloud(
  words = str_c('#',hashtags.unnested.count$hashtag), 
  freq = hashtags.unnested.count$n, 
  min.freq = 40, 
  colors=brewer.pal(8, 'Dark2')
)


plt <- hashtags.unnested.df %>% 
  filter(hashtag %in% c('hoyvotosi', 'votono')) %>% 
  count(Created_At_Round, hashtag) %>% 
  ggplot(mapping = aes(x  = Created_At_Round, y = n, color = hashtag)) +
  theme_light() + 
  xlab(label = 'Date') +
  ggtitle(label = 'Top Hastags Counts') +
  geom_line() + 
  scale_color_manual(values = c('hoyvotosi' = 'green3', 'votono' = 'red'))

plt %>% ggplotly()

#-------------------------------------------------------------------
# Network analysis

bi.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = Text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words %>% 
  select(bigram) %>% 
  head(10)

bi.gram.words %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2))


bi.gram.count <- bi.gram.words %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count %>% head()


# Weight distribution

bi.gram.count %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")


bi.gram.count %>% 
  mutate(weight = log(weight + 1)) %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram log-Weight Distribution")

threshold <- 280

# For visualization purposes we scale by a global factor. 
ScaleWeight <- function(x, lambda) {
  x / lambda
}

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

# Visualization

plot(
  network, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)


# Additional information
# Store the degree.
V(network)$degree <- strength(graph = network)

# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

plot(
  network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)


# Get all connected components.
clusters(graph = network)


# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

cc.network 

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)

# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

plot(
  cc.network, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(cc.network)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(cc.network)$width ,
  main = 'Bigram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)


# Dynamic network

# Treshold
threshold <- 250

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)


# More complex

# Treshold
threshold <- 80

network <-  bi.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

#-------------------------------------------------------------------
# Skipgram analysis

skip.window <- 2

skip.gram.words <- tweets.df %>% 
  unnest_tokens(
    input = Text, 
    output = skipgram, 
    token = 'skip_ngrams', 
    n = skip.window
  ) %>% 
  filter(! is.na(skipgram))

tweets.df %>% 
  slice(4) %>% 
  pull(Text)

skip.gram.words %>% 
  select(skipgram) %>% 
  slice(10:20)

skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
  map_int(.f = ~ ngram::wordcount(.x))

skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)

skip.gram.words %<>% 
  separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

skip.gram.count <- skip.gram.words  %>% 
  count(word1, word2, sort = TRUE) %>% 
  rename(weight = n)

skip.gram.count %>% head()


# Treshold
threshold <- 80

network <-  skip.gram.count %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Select biggest connected component.  
V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)

# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)
# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = cc.network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(cc.network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(cc.network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

#-------------------------------------------------------------------
# Community detection

comm.det.obj <- cluster_louvain(
  graph = cc.network, 
  weights = E(cc.network)$weight
)

comm.det.obj

V(cc.network)$membership <- membership(comm.det.obj)

# We use the membership label to color the nodes.
network.D3$nodes$Group <- V(cc.network)$membership

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

#-------------------------------------------------------------------
# Correlation analysis

cor.words <- words.df %>% 
  group_by(word) %>% 
  filter(n() > 10) %>% 
  pairwise_cor(item = word, feature = ID) 

topic.words <- c('uribe', 'santos', 'farc')


# Set correlation threshold. 
threshold = 0.1

network <- cor.words %>%
  rename(weight = correlation) %>% 
  # filter for relevant nodes.
  filter((item1 %in% topic.words | item2 %in% topic.words)) %>% 
  filter(weight > threshold) %>%
  graph_from_data_frame()

V(network)$degree <- strength(graph = network)

E(network)$width <- E(network)$weight/max(E(network)$weight)

network.D3 <- igraph_to_networkD3(g = network)

network.D3$nodes %<>% mutate(Degree = 5*V(network)$degree)

# Define color groups. 
network.D3$nodes$Group <- network.D3$nodes$name %>% 
  as.character() %>% 
  map_dbl(.f = function(name) {
    index <- which(name == topic.words) 
    ifelse(
      test = length(index) > 0,
      yes = index, 
      no = 0
    )
  }
  )

network.D3$links %<>% mutate(Width = 10*E(network)$width)

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  # We color the nodes using JavaScript code.
  colourScale = JS('d3.scaleOrdinal().domain([0,1,2]).range(["gray", "blue", "red", "black"])'), 
  opacity = 0.8,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We define edge properties using JavaScript code.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  linkDistance = JS("function(d) { return 550/(d.value + 1); }"), 
  fontSize = 18,
  zoom = TRUE, 
  opacityNoHover = 1
)