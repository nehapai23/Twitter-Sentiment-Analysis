setwd("F:/R 2")
library(devtools)
library(twitteR)
library(plyr)
library(dplyr)
library(ggplot2)
api_key <-"___"
api_secret <-"___"
access_token <-"___"
access_token_secret <-"___"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
s <-searchTwitter('#brexit',
	n = 15000,
	since = '20160723',
	locale = 'en',
	geocode = '51.507351,0.127758,100mi')
search <-function(searchterm)
{
	list <-s
	df <-twListToDF(list)
	df <-df[,order(names(df))]
	df$created <-strftime(df$created, '%Y%m%d')
	if (file.exists(paste(searchterm, '_stack.csv')) == FALSE)
	write.csv(df,file = paste(searchterm, '_stack.csv'),row.names = F)
	stack <-read.csv(file = paste(searchterm, '_stack.csv'))
	stack <-rbind(stack, df)
	stack <-subset(stack, !duplicated(stack$text))
	write.csv(stack,file = paste(searchterm, '_stack.csv'),row.names = F)
	score.sentiment <-function(sentences,pos.words,neg.words,.progress = 'none')
	{
		require(plyr)
		require(stringr)
		scores <-laply(sentences, function(sentence, pos.words, neg.words) {
			sentence <-gsub('[[:punct:]]', "", sentence)
			sentence <-gsub('[[:cntrl:]]', "", sentence)
			sentence <-gsub('\\d+', "", sentence)
			sentence <-tolower(sentence)
			word.list <-str_split(sentence, '\\s+')
			words <-unlist(word.list)
			pos.matches <-match(words, pos.words)
			neg.matches <-match(words, neg.words)
			pos.matches <-!is.na(pos.matches)
			neg.matches <-!is.na(neg.matches)
			score <-sum(pos.matches) - sum(neg.matches)
			return(score)
			}, pos.words, neg.words, .progress = .progress)
		scores.df <-data.frame(score = scores, text = sentences)
		return(scores.df)
	}
	pos <-scan('C:/positivewords.txt',what = 'character',comment.char = ';') #folder with positive dictionary
	neg <-scan('C:/negativewords.txt',what = 'character',comment.char = ';') #folder with negative dictionary
	pos.words <-c(pos, 'upgrade')
	neg.words <-c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
	Dataset <-stack
	Dataset$text <-as.factor(Dataset$text)
	scores <-score.sentiment(Dataset$text, pos.words, neg.words, .progress = 'text')
	write.csv(scores,file = paste(searchterm, '_scores.csv'),row.names = TRUE)
	stat <-scores
	stat$created <-stack$created
	stat$created <-as.Date(stat$created)
	stat <-mutate(stat, tweet = ifelse(stat$score > 0,'positive',ifelse(stat$score <- 0, 'negative', 'neutral')))
	by.tweet <-group_by(stat, tweet, created)
	by.tweet <-summarise(by.tweet, number = n())
	write.csv(by.tweet,	file = paste(searchterm, '_opin.csv'),row.names = TRUE)
	ggplot(by.tweet, aes(created, number)) + geom_line(aes(group = tweet, color = tweet), size = 2) +
	geom_point(aes(group = tweet, color = tweet), size = 4) + theme(text = element_text(size = 18),
	axis.text.x = element_text(angle = 90, vjust = 1)) + ggtitle(searchterm)
	ggsave(file = paste(searchterm, '_plot.jpeg'))
}
search("#brexit") #Enter Keyword.