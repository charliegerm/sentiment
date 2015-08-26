# load package
library(twitteR)
library(plyr)
library(ggplot2)

# go authenticate session
twitter_oath()

# get 1500 most recent tweets
united.tweets = searchTwitter('@united', n=1500)
american.tweets = searchTwitter('@AmericanAir', n=1500)
southwest.tweets = searchTwitter('@SouthwestAir', n=1500)
vaa.tweets = searchTwitter ('@VirginAtlantic', n=1500)
metro.tweets = searchTwitter('#wmata', n=1500)

# to examine specific tweets...
tweet = united.tweets[[1]]
tweet$getScreenName()
tweet$getText()

# extract the text of the tweets
united.text <- laply(united.tweets, function(t) t$getText())
american.text = laply(american.tweets, function(t) t$getText())
southwest.text = laply(southwest.tweets, function(t) t$getText())
vaa.text = laply(vaa.tweets, function(t) t$getText())
metro.text = laply(metro.tweets, function(t) t$getText())

# score the sentiment for each tweet
united.scores <- score.sentiment(united.text)
united.scores$airline = 'United'
united.scores$code = 'UA'
american.scores = score.sentiment(american.text)
american.scores$airline = 'American'
american.scores$code = 'AA'
southwest.scores = score.sentiment(southwest.text)
southwest.scores$airline = 'Southwest'
southwest.scores$code = 'WN'
vaa.scores = score.sentiment(vaa.text)
vaa.scores$airline = 'Virgin Atlantic'
vaa.scores$code = 'VS'
metro.scores = score.sentiment(metro.text)
metro.scores$airline = 'Metro'
metro.scores$code = 'WMATA'

# plot each airline's score distribution
hist(united.scores$score)
hist(american.scores$score)
hist(southwest.scores$score)
hist(vaa.scores$score)
hist(metro.scores$score)

# ...or, we can plot it
qplot(united.scores$score)
qplot(american.scores$score)
qplot(southwest.scores$score)
qplot(vaa.scores$score)
qplot(metro.scores$score)

# combine all scores into a single dataframe
all.scores = rbind(united.scores, american.scores, southwest.scores, vaa.scores, metro.scores)

# ggplot2 builds plots in layers
ggplot(data = all.scores) +
  geom_bar(mapping=aes(x=score, fill=airline), binwidth= 1) +
  facet_grid(airline~.) +   # make a separate plot for each airline
  theme_bw() + scale_fill_brewer()

# focus on very good or very bad tweets, ignoring the neutral ones
all.scores$very.pos = as.numeric(all.scores$score >= 2)
all.scores$very.neg = as.numeric(all.scores$score <= -2)

# use ration of very positive to very negative to derive overall sentiment
twitter.df = ddply(all.scores, c('airline', 'code'), summarise, pos.count = sum(very.pos), neg.count = sum(very.neg))

twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round(100 * twitter.df$pos.count / twitter.df$all.count)

orderBy(~-score, twitter.df)







