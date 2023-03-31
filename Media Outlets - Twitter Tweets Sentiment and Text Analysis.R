options(scipen=999)


# #importing packages
library(dotenv)
load_dot_env()
library(twitteR)
library(writexl)
library(bitops)
library(tidytext)
library(devtools)
library(sentiment)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

# ########### Set up Twitter Authentication
consumer_key<-Sys.getenv("CONSUMER_KEY") #API KEY from twitter
consumer_secret<-Sys.getenv("CONSUMER_SECRET") #API KEY SECRET from twitter
access_token<-Sys.getenv("ACCESS_TOKEN") #Access token from twitter
access_token_secret<-Sys.getenv("ACCESS_TOKEN_SECRET") #Access token secret from twitter

#TwitteR Function
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)


encodeSource <- function(x) {
  if(x=="<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>"){
    gsub("<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>", "iphone", x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>"){
    gsub("<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>","ipad",x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>"){
    gsub("<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>","android",x,fixed=TRUE)
  } else if(x=="<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>"){
    gsub("<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>","Web",x,fixed=TRUE)
  } else if(x=="<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>"){
    gsub("<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>","windows phone",x,fixed=TRUE)
  }else if(x=="<a href=\"http://dlvr.it\" rel=\"nofollow\">dlvr.it</a>"){
    gsub("<a href=\"http://dlvr.it\" rel=\"nofollow\">dlvr.it</a>","dlvr.it",x,fixed=TRUE)
  }else if(x=="<a href=\"http://ifttt.com\" rel=\"nofollow\">IFTTT</a>"){
    gsub("<a href=\"http://ifttt.com\" rel=\"nofollow\">IFTTT</a>","ifttt",x,fixed=TRUE)
  }else if(x=="<a href=\"http://earthquaketrack.com\" rel=\"nofollow\">EarthquakeTrack.com</a>"){
    gsub("<a href=\"http://earthquaketrack.com\" rel=\"nofollow\">EarthquakeTrack.com</a>","earthquaketrack",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.didyoufeel.it/\" rel=\"nofollow\">Did You Feel It</a>"){
    gsub("<a href=\"http://www.didyoufeel.it/\" rel=\"nofollow\">Did You Feel It</a>","did_you_feel_it",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.mobeezio.com/apps/earthquake\" rel=\"nofollow\">Earthquake Mobile</a>"){
    gsub("<a href=\"http://www.mobeezio.com/apps/earthquake\" rel=\"nofollow\">Earthquake Mobile</a>","earthquake_mobile",x,fixed=TRUE)
  }else if(x=="<a href=\"http://www.facebook.com/twitter\" rel=\"nofollow\">Facebook</a>"){
    gsub("<a href=\"http://www.facebook.com/twitter\" rel=\"nofollow\">Facebook</a>","facebook",x,fixed=TRUE)
  }else {
    "others"
  }
}



###### Getting timelines of different users and combining them

user1 = 'cnnbrk'
user2 = 'nytimes'
user3 = 'TheEconomist'
user4 = 'BBCBreaking'
user5 = 'Reuters'

timeline1 = twListToDF(userTimeline(user1, n = 1000))
timeline2 = twListToDF(userTimeline(user2, n = 1000))
timeline3 = twListToDF(userTimeline(user3, n = 1000))
timeline4 = twListToDF(userTimeline(user4, n = 1000))
timeline5 = twListToDF(userTimeline(user5, n = 1000))

Tweets_Scrape_Results = rbind(timeline1, timeline2, timeline3, timeline4, timeline5)

# Adding the tweet device source column to your dataframe
Tweets_Scrape_Results$tweetSource = sapply(Tweets_Scrape_Results$statusSource,function(sourceSystem) encodeSource(sourceSystem))

######### Sentiment Analysis
# 4 Positive
# 2 Neutral
# 0 Negative

Tweets_Sentiment<-sentiment(Tweets_Scrape_Results$text)
# Adding the tweet polarity column to your dataframe
Tweets_Scrape_Results$polarity <- Tweets_Sentiment$polarity

#extract results
write_xlsx(Tweets_Scrape_Results,'Tweets_Scrape_Results.xlsx')


########## Text Analysis

#This line will create our body of text to analyze
# lets create our corpus first

our_corpus <- corpus(Tweets_Scrape_Results,text_field = 'text')
ndoc(our_corpus)

#Lets tokenize the corpus

our_tokens <- tokens(our_corpus, remove_punct = TRUE, remove_symbols = TRUE)
stopwords('en')

#remove the stopwords from the tokens
words_to_be_removed<-c()
our_tokens_nonstopwords<- tokens_select(our_tokens, pattern = c(stopwords('en'), words_to_be_removed),selection = 'remove', min_nchar = 6)

#now we will construct document feature matrix which will show the frequency of tokens in each document
our_matrix<-dfm(our_tokens_nonstopwords)
our_matrix<-dfm_trim(our_matrix, min_termfreq = 10, min_docfreq = 5)
nfeat(our_matrix) #number of tokens in all documents


#lets look at the most mentioned words
topfeatures(our_matrix,10)
topfeatures(our_matrix,25)

freq <- textstat_frequency(our_matrix)
head(freq,20)
write_xlsx(freq,'Tweets_Scrape_Word_Freq.xlsx')

############ Visualisations

set.seed(42)

#BASIC Wordcloud
textplot_wordcloud(our_matrix, max_words = 200, min_size = 1, max_size = 5)

#Wordcloud subdivided by company name
corpus_subset(our_corpus, screenName %in% c('cnnbrk', 'nytimes', 'BBCBreaking', 'TheEconomist','Reuters')) %>%
  tokens(remove_punct = TRUE,remove_symbols = TRUE) %>%
  tokens_select(pattern = c(stopwords('en'), words_to_be_removed),selection = 'remove', min_nchar = 6) %>%
  dfm() %>%
  dfm_group(groups = screenName) %>%
  dfm_trim(min_termfreq = 10, min_docfreq = 5) %>%
  textplot_wordcloud(comparison = TRUE, max_words = 50)

#Wordcloud color-coded based on word importance
textplot_wordcloud(our_matrix, min_count = 10,
                   color = c('magenta', 'green', 'blue', 'red'),
                   comparison = FALSE, max_words = 100)

#Lexical Dispersion graph showing the use of Covid in our corpus
our_corpus_subset <- corpus_subset(our_corpus, screenName %in% c('cnnbrk', 'nytimes', 'BBCBreaking', 'TheEconomist','Reuters'))
kwic(tokens(our_corpus_subset), pattern = "Covid") %>%
  textplot_xray()


textplot_xray(
  kwic(our_corpus_subset, pattern = "Covid"),
  kwic(our_corpus_subset, pattern = "Climate")
)


# Only select tweets by CNN Breaking News and NY Times as they are the most followed twitter news accounts
NEWS_corpus <- corpus_subset(our_corpus,
                            screenName %in% c("cnnbrk", "nytimes"))

# Create a dfm grouped by selected companies
NEWS_dfm <- tokens(NEWS_corpus, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = screenName) %>%
  dfm()

# Calculate keyness and determine NY Times as target group
result_keyness <- textstat_keyness(NEWS_dfm, target = "nytimes")

# Plot estimated word keyness
textplot_keyness(result_keyness)
