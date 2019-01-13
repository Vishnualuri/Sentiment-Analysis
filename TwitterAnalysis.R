library("twitteR")
#library("base64enc")
#library("httpuv")
library("ROAuth")



# Get stock details from stock market API
install.packages("BatchGetSymbols")
library(BatchGetSymbols, quietly = T)
first.date <- Sys.Date()-30
last.date <- Sys.Date()

tickers <- c("BAC","JPM","WFC","C","USB","GS")
stocks_full_name <- c("BankofAmerica","jpmorgan","WellsFargo","Citi","usbank","GoldmanSachs")


stocks_data <- BatchGetSymbols(tickers = tickers,
                               first.date = first.date,
                               last.date = last.date)


#Find gainers and losers
# lets find difference between opening and ending prices
#typeof(stocks_data)
#length(stocks_data)

stocks_data$df.tickers
View(stocks_data$df.tickers)

Ordered_stock_data <- stocks_data$df.tickers[order(stocks_data$df.tickers$ref.date, decreasing = T),][1:6,]

#find differnece for losers and gainers
Ordered_stock_data$difference <- Ordered_stock_data$price.open - Ordered_stock_data$price.close
Ordered_stock_data$Fullname <- stocks_full_name


#sort(Ordered_stock_data$difference, decreasing = T)
Ordered_stock_data <- Ordered_stock_data[order(Ordered_stock_data$difference, decreasing = T),]

gainers <- Ordered_stock_data[1:3,]
losers <- Ordered_stock_data[4:6,]


#Get twitter data for the banks

t.api.key <- "b3TpHR0Fq7nO3g2jA6ldwmLRT"
t.api.secret<- "iHun4rRl8JZoHaLeO7kxscRPY9Hp8GVu9HmlVv4zDTOEcpUeFB"
access_token <- '404740991-wrG5k1zPZgSJlDQICRV0kaENsUtxmNdVerVeGZn4'
access_secret <- 'DPlw01Ie4VSI5pRocjtRzPdeSz2cNlm6VqqOEkz1zjAAb'

#setup_twitter_oauth(t.api.key, t.api.secret, access_token=NULL, access_secret=NULL)
setup_twitter_oauth(t.api.key, t.api.secret, access_token, access_secret)

#searchTwitter("virat kholi", n=200)
#gainers tweets
BankofAmerica_tweets <- (twListToDF(searchTwitter(gainers$Fullname[1],n = 200)))$text
usbank_tweets <- (twListToDF(searchTwitter(gainers$Fullname[2],n = 200)))$text
Citi_tweets <- (twListToDF(searchTwitter(gainers$Fullname[3],n = 200)))$text


#(twListToDF(searchTwitter(gainers$Fullname[1],n = 200)))$text



#losers tweets
WellsFargo_tweets <- twListToDF(searchTwitter(losers$Fullname[1],n = 200))$text
jpmorgan_tweets <- twListToDF(searchTwitter(losers$Fullname[2],n = 200))$text
GoldmanSachs_tweets <- twListToDF(searchTwitter(losers$Fullname[3],n = 200))$text


#corpus,dtm and wordcloud
library(tm)
library(SnowballC)

getCorpus <- function(tweets){
  #tweets.text <- lapply(tweets, function(t){t$getText()})
  data.source <- VectorSource(tweets)
  data.corpus <- Corpus(data.source)
  return(data.corpus)
}

gainer_tweets <- c(BankofAmerica_tweets,usbank_tweets,Citi_tweets)
loser_tweets <- c(WellsFargo_tweets,jpmorgan_tweets,GoldmanSachs_tweets)

gainer_corpus <- getCorpus(gainer_tweets)
loser_corpus <- getCorpus(loser_tweets)

#preprocessing
removeURL <- function(x){
  gsub("(http[^ ]*)","",x)
}

removeNumbersWords <- function(x){
  gsub("([[:digit:]]+0=)[[:alnum:]])*","",x)
  
}

for_ascii_char <- function(t){
  iconv(t, "latin1", "ASCII", sub="")
}

getTransCorpus <- function(data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus, content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumbersWords))
  data.corpus <- tm_map(data.corpus, content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus, content_transformer(stripWhitespace))
  data.corpus <- tm_map(data.corpus, content_transformer(for_ascii_char))
  return(data.corpus)
}

data.trans.corpus1 <- getTransCorpus(gainer_corpus)
data.trans.corpus2 <- getTransCorpus(loser_corpus)

#doc term matrix

tdm1 <- TermDocumentMatrix(data.trans.corpus1)
tdm2 <- TermDocumentMatrix(data.trans.corpus2)

# word freq
freq_terms1 <- findFreqTerms(tdm1, lowfreq = 2)
freq_terms2 <- findFreqTerms(tdm2, lowfreq = 2)
word_freq1 <- rowSums(as.matrix(tdm1))
word_freq2 <- rowSums(as.matrix(tdm2))

#wordcloud
#install.packages("wordcloud")
library(wordcloud)
palette <- brewer.pal(8,"Dark2")
set.seed(123)
#freq_terms1
wordcloud(words = names(word_freq1), freq = word_freq1, min.freq = 2, random.order = F, colors = palette)
wordcloud(words = names(word_freq2), freq = word_freq2, min.freq = 2, random.order = F, colors = palette)

#Sentiment analysis
sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

#lexicon
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

#gainer analysis

tweets1 <- gainer_tweets
save(list="tweets1", file="tweets1.RData")
load(file="tweets1.RData")
#tweets1.texts <- lapply(tweets1, function(t) {iconv(t$getText(),"latin1", "ASCII", sub="")})
sentiment_gainer <- sentiment.na(tweets1,pos.words,neg.words)
scores_gainer.na <- sapply(tweets1, sentiment.na, pos.words,neg.words)
table(scores_gainer.na)

#loser analysis
tweets2 <- loser_tweets
save(list = "tweets2", file = "tweets2.RData")
load(file="tweets2.Rdata")
sentiment_loser <- sentiment.na(tweets2, pos.words, neg.words)
scores_loser.na <- sapply(tweets2, sentiment.na, pos.words,neg.words)

table(scores_loser.na)

#Gainer tweets plot

tweets1.vector <- sapply(tweets1, function (t) {(t)})
x <- data.frame(score=scores_gainer.na, Text=tweets1.vector)

for(i in 1:nrow(x)){
  if(is.na(x$score[i])) { 
    x$sentiment[i] <- c("neutral")
  }else if(x$score[i] > 0) {
    x$sentiment[i] <- c("positive")
  }else if (x$score[i] < 0) {
    x$sentiment[i] <- c("negative")
  }else x$sentiment[i] <- c("neutral")
  
}

gainers_name <- paste(as.character(Ordered_stock_data$Full.name[1]),
                      as.character(Ordered_stock_data$Full.name[2]),
                      as.character(Ordered_stock_data$Full.name[3]), sep=",")

library(ggplot2)
gbar_gainers <- ggplot(x, aes(x = score, y = 1, fill = sentiment))

gbar_gainers + geom_bar(stat = 'identity') + ggtitle(paste("Gainers Sentiment",gainers_name)) + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
        axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1))


#losers tweets plot
tweets2.vector <- sapply(tweets2, function (t) {(t)})
y <- data.frame(score=scores_loser.na, Text=tweets2.vector)

for(i in 1:nrow(y)){
  if(is.na(y$score[i])) { 
    y$sentiment[i] <- c("neutral")
  }else if(y$score[i] > 0) {
    y$sentiment[i] <- c("positive")
  }else if (y$score[i] < 0) {
    y$sentiment[i] <- c("negative")
  }else y$sentiment[i] <- c("neutral")
  
}

gbar_loser <- ggplot(y, aes(x = score, y = 1, fill = sentiment))

losers_name <- paste(as.character(Ordered_stock_data$Full.name[4]),
                     as.character(Ordered_stock_data$Full.name[5]),
                     as.character(Ordered_stock_data$Full.name[6]), sep=",")


gbar_loser + geom_bar(stat = 'identity') + ggtitle(paste("Loser Sentiment : ",losers_name)) + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
        axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -1))

#candlestick plot
library(googleVis)
stock_day_chart_1 <- gvisCandlestickChart(Ordered_stock_data,xvar= "Fullname",
                                          low="price.low", high="price.high", open="price.open",
                                          close="price.close", 
                                          options= list (vAxis='{minValue:0, maxValue:100}', legend="top", height=1000, width=1200))

plot(stock_day_chart_1)



