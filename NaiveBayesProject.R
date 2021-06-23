library(twitteR)
library(devtools)
library(RSQLite)
if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(Rstem)
library(sentiment)
library(plotly)
library(dplyr)
library(wordcloud)
library(xlsx)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

setup_twitter_oauth("XXX","XXX","XXX","XXX")

register_sqlite_backend("sqlite bremen monchengladbach")

tweets_db = search_twitter_and_store("XXX", lang='en', since='2020-05-27', until='2020-05-28', retryOnRateLimit = 20)

some_tweets = load_tweets_db()

f_clean_tweets <- function (tweets) {
  
  clean_tweets = sapply(tweets, function(x) x$getText())
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  # remove emojis or special characters
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}

clean_tweets <- f_clean_tweets(some_tweets)

# removing duplicates due to retweets
clean_tweets <- clean_tweets[!duplicated(clean_tweets)]

# using sentiment package to classify emotions
emotions <- classify_emotion(clean_tweets, algorithm='bayes')

# using sentiment package to classify polarities
polarities = classify_polarity(clean_tweets, algorithm='bayes')

df = data.frame(text=clean_tweets, emotion=emotions[,'BEST_FIT'],
                polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."

# plot the emotions
plot_ly(df, x=~emotion,type="histogram",
        marker = list(color = c('grey', 'red',
                                'orange', 'navy',
                                'yellow'))) %>%
  layout(yaxis = list(title='Count'), title="Sentiment Analysis: Emotions")

plot_ly(df, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Count'), title="Sentiment Analysis: Polarity")

# Visualize the words by polarity
df <- df %>%
  group_by(polarity) %>%
  summarise(pasted=paste(text, collapse=" "))

# remove stopwords
df$pasted = removeWords(df$pasted, stopwords('english'))

# create corpus
corpus = Corpus(VectorSource(df$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = df$polarity

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(3, 'Dark2'),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

write.xlsx(tdm, "tdm bremen monchengladbach.xlsx")