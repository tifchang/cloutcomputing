library(tm)
library(dplyr)
library(wordcloud)
library(data.table)
library(syuzhet) 
library(ggplot2)
library(plotly)
library(topicmodels)
library(rvest)
library(stringr)
library(httr)
library(magrittr)
library(twitteR)
library(anytime)

consumer_key = "gxOUpkuB7sLWsB1RI3KfRqjPu"
consumer_secret = "eW3fQUkBHdZM24cOJTOeEug3IsEcxsi0nBjecLdqKJp6QhcYpI"
access_token =  "20756072-Rw8kAzGryUxBi41UeLdpP1rFZ4PuSdRtAlwHWCU6L"
access_secret = "UFzfhAnoylwbKHfNJiEf9RiIkoZSWdNiDIK8KuAHfcaPk"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#searchTwitter(paste0('from:', "@kimkardashian"), n=100)

# Create df of Justin Bieber's Tweets
j_tweets = userTimeline("justinbieber", n=500, includeRts = F, excludeReplies = T)
k_tweets = userTimeline("kimkardashian", n=500, includeRts = F, excludeReplies = T)
b_tweets = userTimeline("BarackObama", n=500, includeRts = F, excludeReplies = T)
d_tweets = userTimeline("realdonaldtrump", n=500, includeRts = F, excludeReplies = T)

j= NULL
j = cbind(j, sapply(j_tweets, function (x) x$getRetweetCount()))
j = cbind(j, sapply(j_tweets, function (x) x$getText()))
j = cbind(j, sapply(j_tweets, function (x) x$getScreenName()))
j = cbind(j, sapply(j_tweets, function (x) x$getId()))
j = cbind(j, sapply(j_tweets, function (x) x$getCreated()))
j = cbind(j, ifelse(str_detect(j[,2], "https"), 1, 0))
jdf = as.data.frame(j)
names(jdf) = c("Retweet Count", "Content", "Author", "Id", "Created", "Contains Link")
jdf$Content = gsub("[^[:alnum:]///' ]", "", jdf$Content)
jdf$Datetime = anytime(jdf$Created)
View(jdf)

jcorp.original <- VCorpus(VectorSource(jdf$Content))
jcorp = tm_map(jcorp.original, removePunctuation) 
jcorp = tm_map(jcorp, removeNumbers) 
jcorp = tm_map(jcorp, content_transformer(tolower) ,lazy=TRUE) 
jcorp = tm_map(jcorp, content_transformer(removeWords), stopwords("english") ,lazy=TRUE)
jcorp = tm_map(jcorp, content_transformer(stemDocument) ,lazy=TRUE) 
jcorp = tm_map(jcorp, stripWhitespace, lazy=TRUE)
jdtm = DocumentTermMatrix(jcorp)
jdtm_matrix = as.matrix(jdtm)

jsentiment.avg = mean(get_sentiment(jdf$Content, method="afinn"))
jsentiment = get_sentiment(jdf$Content, method="afinn")
js_df = as.data.frame(jsentiment)
js_df$Tweet = c(1:nrow(js_df))
names(js_df)[1] = "Emotion"
ggplot(js_df, aes(x=Tweet, y=Emotion)) + geom_point() + stat_smooth(colour="#f22e2e") + geom_hline(yintercept=0)

# Create df of Kim Kardashian's Tweets
k = cbind(k, sapply(k_tweets, function (x) x$getRetweetCount()))
k = cbind(k, sapply(k_tweets, function (x) x$getText()))
k = cbind(k, sapply(k_tweets, function (x) x$getScreenName()))
k = cbind(k, sapply(k_tweets, function (x) x$getId()))
k = cbind(k, sapply(k_tweets, function (x) x$getCreated()))
kdf = as.data.frame(k)
names(kdf) = c("Retweet Count", "Content", "Author", "Id", "Created")
View(kdf)

# Create df of Barack Obama's Tweets
b = NULL
b = cbind(b, sapply(b_tweets, function (x) x$getRetweetCount()))
b = cbind(b, sapply(b_tweets, function (x) x$getText()))
b = cbind(b, sapply(b_tweets, function (x) x$getScreenName()))
b = cbind(b, sapply(b_tweets, function (x) x$getId()))
b = cbind(b, sapply(b_tweets, function (x) x$getCreated()))
bdf = as.data.frame(b)
names(bdf) = c("Retweet Count", "Content", "Author", "Id", "Created")
View(bdf)

# Create df of Donald Trump's Tweets
d = NULL
d = cbind(d, sapply(d_tweets, function (x) x$getRetweetCount()))
d = cbind(d, sapply(d_tweets, function (x) x$getText()))
d = cbind(d, sapply(d_tweets, function (x) x$getScreenName()))
d = cbind(d, sapply(d_tweets, function (x) x$getId()))
d = cbind(d, sapply(d_tweets, function (x) x$getCreated()))
d = cbind(d, ifelse(str_detect(j[,2], "https"), 1, 0))
ddf = as.data.frame(d)
names(ddf) = c("Retweet Count", "Content", "Author", "Id", "Created", "Link")
ddf$Content = gsub("[^[:alnum:]///' ]", "", ddf$Content)
View(ddf)

dcorp.original <- VCorpus(VectorSource(ddf$Content))
dcorp = tm_map(dcorp.original, removePunctuation) 
dcorp = tm_map(dcorp, removeNumbers) 
dcorp = tm_map(dcorp, content_transformer(tolower) ,lazy=TRUE) 
dcorp = tm_map(dcorp, content_transformer(removeWords), stopwords("english") ,lazy=TRUE)
dcorp = tm_map(dcorp, content_transformer(stemDocument) ,lazy=TRUE) 
dcorp = tm_map(dcorp, stripWhitespace, lazy=TRUE)
ddtm = DocumentTermMatrix(dcorp)
ddtm_matrix = as.matrix(ddtm)

dsentiment.avg = mean(get_sentiment(ddf$Content, method="afinn"))
dsentiment = get_sentiment(ddf$Content, method="afinn")
ds_df = as.data.frame(dsentiment)
#ds_df$Tweet = c(1:330)
#names(ds_df)[1] = "Emotion"
ggplot(ds_df, aes(x=Tweet, y=Emotion)) + geom_point() + stat_smooth(colour="#C6FF00") + geom_hline(yintercept=0)

c = read.csv("tiff_created.csv")
cdf = as.data.frame(c)
cdf$Y = anytime(cdf$x)
