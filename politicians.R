# word blobs

if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
if (!require(slam)) {
  install.packages("slam", repos="http://cran.us.r-project.org")
}
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}
if (!require(xts)) {
  install.packages("xts", repos="http://cran.us.r-project.org")
}
if (!require(textclean)) {
  install.packages("textclean", repos="http://cran.us.r-project.org")
}
if (!require(tidytext)) {
  install.packages("tidytext", repos="http://cran.us.r-project.org")
}


library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(xts)
library(textclean)
library(tidytext)
library(syuzhet)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sentimentr, dplyr, magrittr)
library('sentimentr')

workingDir = '/Users/michaeltauberg/projects/NewsScraper'
csvName = "politicians7.csv"
data_name = "politicians"
setwd(workingDir)
#options(encoding="latin1")
#dt = read.csv(file=csvName, stringsAsFactors=FALSE, fileEncoding="utf-8")
dt = read.csv(file=csvName)

dt = dt[!duplicated(dt[,c('headline')], fromLast=FALSE),]
dt$headline = tolower(dt$headline)
dt$date = as.Date(dt$date)
dt$num_articles = rep(1,nrow(dt))

# define  news sources
all = c( "politico","huff","buzzfeed", "cnn", "msnbc", "vox", "npr",
        "nytimes", "wsj", "latimes", "usatoday", "washingtonpost", "bostonglobe",
        "foxnews", "dailycaller", "infowars", "breitbart")

online = c("cnn", "msnbc", "npr", "vox")

#https://www.journalism.org/2014/10/21/political-polarization-media-habits/
liberal = c("npr", "politico","huff","buzzfeed", "cnn", "msnbc", "nytimes")
newspapers = c("nytimes", "wsj", "latimes", "usatoday", "washingtonpost", "bostonglobe")
conservative = c("foxnews", "dailycaller", "breitbart")
cnn = c("cnn") # seems too low
nytimes = c("nytimes")

# list the candidates 
politicians = c("harris", "warren", "sanders", "yang", "biden", "beto", "booker",
                "gillibrand", "klobuchar", "castro", "gabbard", "buttigieg")

# do word2vec later

# remove stories not about politicians- ie warren buffet stories (not elizabeth warren)
dt = dt[!grepl("Buffet", dt$content),]
dt = dt[!grepl("buffet", dt$headline),]
dt = dt[!grepl("harrison", dt$headline),]
dt = dt[!grepl("Harrison", dt$content),]
dt = dt[!grepl("Joe Harris", dt$headline),]
dt = dt[!grepl("joe harris", dt$content),]
dt = dt[!grepl("fidel", dt$headline),]
dt = dt[!grepl("Fidel", dt$content),]
dt = dt[!grepl("Raul", dt$content),]
dt = dt[!grepl("raul", dt$headline),]
dt = dt[!grepl("huckabee", dt$headline),]
dt = dt[!grepl("Huckabee", dt$content),]
dt = dt[!grepl("aubameyang", dt$headline),]
dt = dt[!grepl("Aubameyang", dt$content),]
dt = dt[!grepl("pyongyang", dt$headline),]
dt = dt[!grepl("hengjun", dt$headline),]
dt = dt[!grepl("yangtze", dt$headline),]
dt = dt[!grepl("yang-ho", dt$headline),]


# wee need a way to find if more than on is mentioned . If more than one, add it to the first one
# ie if "harris slams beto", it's a harris article
# for article in articles
#  if we find a name of another candidate, then remove it????  more fair

liberal_articles = data.frame()
for (source in liberal) {
  print(source)
  source_articles = dt[grep(source, dt$url),]
  liberal_articles = rbind(liberal_articles, source_articles)
  #num_articles = num_articles + nrow(politician_source_articles)
}
conservative_articles = data.frame()
for (source in conservative) {
  print(source)
  source_articles = dt[grep(source, dt$url),]
  conservative_articles = rbind(conservative_articles, source_articles)
  #num_articles = num_articles + nrow(politician_source_articles)
}
newspaper_articles = data.frame()
for (source in newspapers) {
  print(source)
  source_articles = dt[grep(source, dt$url),]
  newspaper_articles = rbind(newspaper_articles, source_articles)
  #num_articles = num_articles + nrow(politician_source_articles)
}


ComparePoliticians <- function(dt, politicians, news_category, name) {
# make this a function also, get avg sentiment of headlines /stories 
  i = 1
  num_source_articles = data.frame(politician=rep("", length(politicians)), num_articles=rep("", length(politicians)), stringsAsFactors=FALSE) 
  for(politician in politicians) {
    print(politician)
    politician_articles =  dt[grep(politician, dt$headline),]
    num_articles = 0
    for (source in news_category) {
      print(source)
      politician_source_articles = politician_articles[grep(source, politician_articles$url),]
      num_articles = num_articles + nrow(politician_source_articles)
    }
    num_source_articles[i,] = c(politician, num_articles)
    i = i + 1
  }
  num_source_articles$num_articles = strtoi(num_source_articles$num_articles)
  num_source_articles$politician = factor(num_source_articles$politician, levels = num_source_articles$politician[order(num_source_articles$num_articles, decreasing=FALSE)])
  num_source_articles = num_source_articles[order(num_source_articles$num_articles, decreasing=TRUE),]
  p = ggplot(num_source_articles, aes(x=politician, y=num_articles, fill="#003b9b")) + geom_bar(stat="identity") 
  p = p + ggtitle(sprintf("%s News Articles per Politician", name))
  p = p + theme(plot.title = element_text(hjust = 0.5))
  #p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
  p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
  p = p + theme(plot.title = element_text(size=18,face="bold"))
  #p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
  #p = p + xlab("Number of Articles") + guides(fill=FALSE)
  p = p + ylab("Number of Articles") + guides(fill=FALSE)
  p = p + coord_flip()
  p = p + scale_fill_manual(values = c("#003b9b"))
  #p = p + scale_fill_manual(values = c("#ad1605"))
  ggsave(filename = sprintf("./%s_articles.png", name) , plot=p, width=8, height=6)
}

ComparePoliticians(dt, politicians, all, "All")
ComparePoliticians(dt, politicians, online, "online")
ComparePoliticians(dt, politicians, liberal, "Liberal")
ComparePoliticians(dt, politicians, newspapers, "Newspapers")
ComparePoliticians(dt, politicians, conservative, "Conservative")
ComparePoliticians(dt, politicians, cnn, "cnn")
ComparePoliticians(dt, politicians, nytimes, "nytimes")

ComparePoliticians(dt, c(politicians, "ocasio-cortez"), all, "AOC")
ComparePoliticians(dt, c(politicians, "trump", "ocasio-cortez"), all, "Trump")
# get conservative news sources


####-------------
#
#
# WORD CLOUDS
#
#
####-------------
GenerateWordClouds <- function(dt_uniq, data_name, color) {
  dt_uniq$content = gsub('\'',"",as.character(dt_uniq$content))
  dt_uniq$content = gsub('\"',"",as.character(dt_uniq$content))
  dt_uniq$content = gsub('\'s',"",as.character(dt_uniq$content))
  
  dt_sent = unnest_tokens(dt_uniq, "sentences", content, token = "sentences")
  # only use sentences that contain the subject - ie "harris"
  subject_words = dt_sent[grep(data_name, dt_sent$sentences),]$sentences
  subject_words = strip(subject_words, apostrophe.remove=TRUE)
  #sentences = get_sentences(raw_words)
  
  words = Corpus(VectorSource(subject_words))
  corpus <- tm_map(words, content_transformer(tolower))
  
  words = tm_map(words, stripWhitespace)
  words = tm_map(words, tolower)
  
  
  complete_stopwords = c(stopwords('english'), "will", "2020", "campaign", "says", "news", "new", "error", "said", "american")
  complete_stopwords = c(stopwords('english'), "harriss", "bidens", "warrens", "betos", "klobuchars", "castros", "gabbards", "yangs", "buttigiegs", "sanderss", "orourkes", "warren")
  complete_stopwords = c(complete_stopwords, "elizabeth", "bernie", "video", "npr", "explained", "sanders", "-", "twoway", "explainer", "—","booker", "bookers")
  complete_stopwords = c(complete_stopwords, "orourke", "yang", "biden", "joe", "beto", "amy", "klobuchar", "castro", "gababrd", "tulsi", "julian", "buttigieg", "andrew")
  complete_stopwords = c(complete_stopwords, "pete", "corey", "cory", "democratic", "candidate", "president", "said", "presidential", "campaign", "kamala", "harris", "new", "run")
  # Generate wordcloud removing all stop words
  png(sprintf("%s_stopwords_wordcloud.png", data_name))
  words = tm_map(words, removeWords, complete_stopwords)
  wordcloud(words, max.words = 40, min.freq=5, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  
  dev.off()
  
  dtm = TermDocumentMatrix(words)
  m = as.matrix(dtm)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = rownames(m), 
                 freq = rowSums(m), 
                 row.names = NULL)
  #d = data.frame(word = names(v),freq=v)
  d = d[order(d$freq, decreasing=TRUE),]
  d$word = factor(d$word, levels = d$word[order(d$freq, decreasing=TRUE)])
  
  top_words = d[1:10,]
  p = ggplot(top_words, aes(x=word, y=freq, fill=data_name)) + geom_bar(stat="identity") 
  p = p + ggtitle(sprintf("%s - Top Words", toupper(data_name)))
  p = p + theme(plot.title = element_text(hjust = 0.5))
  p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
  p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
  p = p + theme(plot.title = element_text(size=18,face="bold"))
  #p = p + xlab("Word")
  p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
  p = p + ylab("Number of Uses") 
  
  #p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
  ggsave(filename = sprintf("./%s_top10.png", data_name) , plot=p, width=4.5, height=6)
}

# search for number of mentions
# get all articles and grep for "elizabeth warren" etc - count number of mentions in all
harris = dt[grep("harris", dt$headline),]
warren = dt[grep("warren", dt$headline),]
sanders = dt[grep("sanders", dt$headline),]
yang = dt[grep("yang", dt$headline),]
biden = dt[grep("biden", dt$headline),]
beto = dt[grep("beto", dt$headline),]
booker = dt[grep("booker", dt$headline),]
gillibrand = dt[grep("gillibrand", dt$headline),]
klobuchar = dt[grep("klobuchar", dt$headline),]
castro = dt[grep("castro", dt$headline),]
gabbard = dt[grep("gabbard", dt$headline),]
buttigieg = dt[grep("buttigieg", dt$headline),]

yang = yang[!grepl("aubameyang", yang$headline),]
yang = yang[!grepl("Aubameyang", yang$headline),]
yang = yang[!grepl("pyongyang", yang$headline),]
yang = yang[!grepl("hengjun", yang$headline),]
yang = yang[!grepl("yangtze", yang$headline),]
yang = yang[!grepl("yang-ho", yang$headline),]

GenerateWordClouds(harris, "harris", "blue")
GenerateWordClouds(warren, "warren", "blue")
GenerateWordClouds(sanders, "sanders", "blue")
GenerateWordClouds(biden, "biden", "blue")
GenerateWordClouds(beto, "beto", "blue")
#GenerateWordClouds(yang, "yang", "blue")
GenerateWordClouds(buttigieg, "buttigieg", "blue")
#GenerateWordClouds(klobuchar, "klobuchar", "blue")
GenerateWordClouds(booker, "booker", "blue")

####-------------
#
#
# ARTICLES OVER TIME
#
#
####-------------

PlotOverTime <- function(dt_candidate, name, weekly_article_stats, max_date) {
  name.ts = xts(dt_candidate$num_articles,dt_candidate$date)
  articles = apply.weekly(name.ts,sum)
  articles = articles[1:length(articles)] # remove last entry for incomplete week
  #dev.on()
  png(sprintf('./%s_time.png', name), width = 880, height = 580,)
  print(plot(articles))
  #dev.copy(png, sprintf('%s_time.png', name))
  dev.off()
  
  num_rows = length(articles)
  num_weekly_articles = data.frame(date=rep("", num_rows), politician=rep(NA, num_rows), num_articles=rep("", num_rows), stringsAsFactors=FALSE) 
  for (i in seq(1:length(articles))) {
    num_weekly_articles[i,] = c(as.character(index(articles)[i]), name, coredata(articles)[i])
    #weekly_article_stats[j,] = num_weekly_articles[i,]
  }
  #j = j + 1
  num_weekly_articles = num_weekly_articles[num_weekly_articles$date < max_date,]
  weekly_article_stats = rbind(weekly_article_stats, num_weekly_articles)
  return(weekly_article_stats)
}

max_date = "2019-04-26"
weekly_article_stats = data.frame()
weekly_article_stats = PlotOverTime(sanders, "sanders", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(harris, "harris", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(warren, "warren", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(beto, "beto", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(biden, "biden", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(yang, "yang", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(booker, "booker", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(gabbard, "gabbard", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(castro, "castro", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(klobuchar, "klobuchar", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(gillibrand, "gillibrand", weekly_article_stats, max_date)
weekly_article_stats = PlotOverTime(buttigieg, "buttigieg", weekly_article_stats, max_date)
#weekly_article_stats = PlotOverTime(dt, "", weekly_article_stats, max_date)

weekly_article_stats$num_articles = strtoi(weekly_article_stats$num_articles)
weekly_article_stats$date = as.Date(weekly_article_stats$date)

#split between top contenders and others
# top contenders
warren = weekly_article_stats[weekly_article_stats$politician == "warren", ]
harris = weekly_article_stats[weekly_article_stats$politician == "harris", ]
sanders = weekly_article_stats[weekly_article_stats$politician == "sanders", ]
biden = weekly_article_stats[weekly_article_stats$politician == "biden", ]
beto = weekly_article_stats[weekly_article_stats$politician == "beto", ]
# bottom contenders
yang = weekly_article_stats[weekly_article_stats$politician == "yang", ]
booker = weekly_article_stats[weekly_article_stats$politician == "booker", ]
gillibrand = weekly_article_stats[weekly_article_stats$politician == "gillibrand", ]
klobuchar = weekly_article_stats[weekly_article_stats$politician == "klobuchar", ]
buttigieg = weekly_article_stats[weekly_article_stats$politician == "buttigieg", ]

beto$politician = rep("o'rourke", nrow(beto))
top_contenders = rbind(warren,harris,sanders, beto, biden, buttigieg)
top_contenders$politician = factor(top_contenders$politician)

data_name = "top_contenders"
p = ggplot(top_contenders, aes(x=date, y=num_articles, color=politician)) +  geom_line(aes(linetype = politician))
#p = p + stat_smooth(aes(x =date, y =num_articles), method = "lm",
#              formula = y ~ poly(x, floor(nrow(top_contenders)/length(levels(top_contenders$politician)))-2), se = FALSE) 
p = p + ggtitle("Frontrunners Num Articles Over Time")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Number of Articles per Week")
p = p + scale_y_continuous(limits = c(0, max(top_contenders$num_articles) + 10))
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_all_over_time.png", data_name) , plot=p, width=8, height=6)


bottom_contenders = rbind(yang, booker, gillibrand, klobuchar, castro, gabbard)
bottom_contenders$politician = factor(bottom_contenders$politician)
data_name = "bottom_contenders"
p = ggplot(bottom_contenders, aes(x=date, y=num_articles, color=politician)) + geom_point() 
p = p + geom_line()
#p = p + stat_smooth(aes(x =date, y =num_articles), method = "lm",
#                    formula = y ~ poly(x, floor(nrow(bottom_contenders)/length(levels(bottom_contenders$politician)))-2), se = FALSE) 
p = p + ggtitle(sprintf("%s - Num Articles Over Time", toupper(data_name)))
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
#p = p + scale_fill_manual(values = c(color)) 
#+ guides(fill=FALSE)
# scale so that 0 is the min 
p = p + ylab("Number of Articles per Week")
p = p + scale_y_continuous(limits = c(0, max(bottom_contenders$num_articles) + 10))
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_all_over_time.png", data_name) , plot=p, width=8, height=6)


####-------------
#
#
# SENTIMENT  scores
#
#
####-------------
# get only articles from liberal sources and do sentiment
politician_sentiments = c()
for (politician in politicians) {
  print(politician)
  politician_articles = liberal_articles[grep(politician, liberal_articles$headline),]
  politician_sentences = unnest_tokens(politician_articles, "sentences", content, token = "sentences")
  # get only sentences with politician name??
  #politician_sentences = politician_sentences[grep(politician, politician_sentences$sentences),]
  politician_sentiment = sentiment(politician_sentences$sentences)
  politician_avg_sentiment = mean(politician_sentiment$sentiment)
  politician_sentiments = append(politician_sentiments, politician_avg_sentiment)
}
sentiments = cbind(politicians, politician_sentiments)
colnames(sentiments) = c("politician", "avg_sentiment")
sentiments = data.frame(sentiments)
sentiments$avg_sentiment = as.numeric(as.character(sentiments$avg_sentiment))
data_name = "liberal_sentiment"
sentiments$politician = factor(sentiments$politician, levels = sentiments$politician[order(sentiments$avg_sentiment, decreasing=FALSE)])
sentiments = sentiments[order(sentiments$avg_sentiment, decreasing=TRUE),]
p = ggplot(sentiments, aes(x=politician, y=avg_sentiment, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("Average Story Sentiment per Candidate from Liberal News Sources")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
#p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + xlab("Avg Sentiment Score") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_sentiment.png", data_name) , plot=p, width=8, height=6)

politician_sentiments = c()
politicians = c("harris", "warren", "sanders", "biden", "beto", "booker",
                "gillibrand", "klobuchar", "castro", "gabbard", "buttigieg")
for (politician in politicians) {
  print(politician)
  politician_articles = liberal_articles[grep(politician, liberal_articles$headline),]
  politician_sentiment = sentiment(politician_articles$headline)
  politician_avg_sentiment = mean(politician_sentiment$sentiment)
  politician_sentiments = append(politician_sentiments, politician_avg_sentiment)
}
sentiments = cbind(politicians, politician_sentiments)
colnames(sentiments) = c("politician", "avg_sentiment")
sentiments = data.frame(sentiments)
sentiments$avg_sentiment = as.numeric(as.character(sentiments$avg_sentiment))
data_name = "liberal_headline_sentiment"
sentiments$politician = factor(sentiments$politician, levels = sentiments$politician[order(sentiments$avg_sentiment, decreasing=FALSE)])
sentiments = sentiments[order(sentiments$avg_sentiment, decreasing=TRUE),]
p = ggplot(sentiments, aes(x=politician, y=avg_sentiment, fill="#003b9b")) + geom_bar(stat="identity") 
p = p + ggtitle("Average Headline Sentiment per Candidate from Liberal News Sources")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
#p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + xlab("Avg Sentiment Score") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_sentiment.png", data_name) , plot=p, width=8, height=6)

# get only articles from conservative sources and do sentiment
politician_sentiments = c()
for (politician in politicians) {
  print(politician)
  politician_articles = conservative_articles[grep(politician, conservative_articles$headline),]
  politician_sentences = unnest_tokens(politician_articles, "sentences", content, token = "sentences")
  # get only sentences with politician name??
  #politician_sentences = politician_sentences[grep(politician, politician_sentences$sentences),]
  politician_sentiment = sentiment(politician_sentences$sentences)
  politician_avg_sentiment = mean(politician_sentiment$sentiment)
  politician_sentiments = append(politician_sentiments, politician_avg_sentiment)
}
sentiments = cbind(politicians, politician_sentiments)
colnames(sentiments) = c("politician", "avg_sentiment")
sentiments = data.frame(sentiments)
sentiments$avg_sentiment = as.numeric(as.character(sentiments$avg_sentiment))
data_name = "conservative_sentiment"
sentiments$politician = factor(sentiments$politician, levels = sentiments$politician[order(sentiments$avg_sentiment, decreasing=TRUE)])
sentiments = sentiments[order(sentiments$avg_sentiment, decreasing=TRUE),]
p = ggplot(sentiments, aes(x=politician, y=avg_sentiment, fill=politician)) + geom_bar(stat="identity") 
p = p + ggtitle("Average Story Sentiment per Candidate from Conservative News Sources")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
#p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + ylab("Avg Sentiment Score") + guides(fill=FALSE)
ggsave(filename = sprintf("./%s_sentiment.png", data_name) , plot=p, width=8, height=6)

politician_sentiments = c()
for (politician in politicians) {
  print(politician)
  politician_articles = conservative_articles[grep(politician, conservative_articles$headline),]
  politician_sentiment = sentiment(politician_articles$headline)
  politician_avg_sentiment = mean(politician_sentiment$sentiment)
  politician_sentiments = append(politician_sentiments, politician_avg_sentiment)
}
sentiments = cbind(politicians, politician_sentiments)
colnames(sentiments) = c("politician", "avg_sentiment")
sentiments = data.frame(sentiments)
sentiments$avg_sentiment = as.numeric(as.character(sentiments$avg_sentiment))
data_name = "conservative_headline_sentiment"
sentiments$politician = factor(sentiments$politician, levels = sentiments$politician[order(sentiments$avg_sentiment, decreasing=TRUE)])
sentiments = sentiments[order(sentiments$avg_sentiment, decreasing=TRUE),]
p = ggplot(sentiments, aes(x=politician, y=avg_sentiment, fill=politician)) + geom_bar(stat="identity") 
p = p + ggtitle("Average Headline Sentiment per Candidate from Conservative News Sources")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=14,face="bold"))
#p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
p = p + ylab("Avg Sentiment Score") + guides(fill=FALSE)
ggsave(filename = sprintf("./%s_sentiment.png", data_name) , plot=p, width=8, height=6)


####-------------
#
#
# Twitter
#
#
####-------------

# new followers
new_followers = read.csv("politician_twitter_newfollowers.csv")
new_followers = new_followers[new_followers$politician != "trump",]
new_followers = new_followers[new_followers$politician != "aoc",]
#new_followers = new_followers[new_followers$politician != "buttigieg",]
#new_followers[new_followers$date == 2,]$date = "Feb" 
#new_followers[new_followers$date == 3,]$date = "March" 
#new_followers[new_followers$date == 4,]$date = "April" 
data_name = "new_twitter_followers"
p = ggplot(new_followers, aes(x=date, y=delta, color=politician)) + geom_point() 
p = p + geom_line()
p = p + ggtitle("New Twitter Followers per Month (millions)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13))
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("New Twitter Followers (millions)")
p = p + xlab("Month")
p = p + scale_x_continuous(breaks = c(2,3,4))
#p + scale_x_discrete(labels=c("2" = "Feb", "3" = "March", "4" = "April"))
ggsave(filename = sprintf("./%s_all_over_time.png", data_name) , plot=p, width=8, height=6)

data_name = "total_twitter_followers"
followers = read.csv("politician_twitter_totalfollowers.csv")
followers = followers[followers$politician != "trump",]
followers = followers[followers$politician != "aoc",]
followers$politician = factor(followers$politician, levels = followers$politician[order(followers$followers, decreasing=TRUE)])
followers = followers[order(followers$followers, decreasing=TRUE),]
p = ggplot(followers, aes(x=politician, y=followers, fill=politician)) + geom_bar(stat="identity") 
p = p + ggtitle("Candidates Total Twitter Followers in April 2019 (millions)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Total Twitter Followers (millions)")
p = p + xlab("Candidate")
ggsave(filename = sprintf("./%s_articles.png", data_name) , plot=p, width=8, height=6)


####-------------
#
#
# Polls
#
#
####-------------
data_name = "polls"
polls = read.csv("polls.csv")
polls$name = factor(polls$name, levels = polls$name[order(polls$poll, decreasing=FALSE)])
p = ggplot(polls, aes(x=name, y=poll, fill="#42b0f4")) + geom_bar(stat="identity") 
p = p + ggtitle("Polling Among Democratic Primary Voters")
p = p + theme(plot.title = element_text(hjust = 0.5))
#p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
#p = p + scale_fill_manual(values = c(color)) + guides(fill=FALSE)
#p = p + xlab("Number of Articles") + guides(fill=FALSE)
p = p + ylab("Poll %") + guides(fill=FALSE)
p = p + coord_flip()
p = p + scale_fill_manual(values = c("#42b0f4"))
ggsave(filename = sprintf("./%s_articles.png", data_name) , plot=p, width=8, height=6)




####-------------
#
#
# Facebook
#
#
####-------------

# new followers
new_followers = read.csv("politician_facebook_newfollowers.csv")
#new_followers = new_followers[new_followers$politician != "buttigieg",]
#new_followers[new_followers$date == 2,]$date = "Feb" 
#new_followers[new_followers$date == 3,]$date = "March" 
#new_followers[new_followers$date == 4,]$date = "April" 
data_name = "new_facebook_followers"
p = ggplot(new_followers, aes(x=month, y=new_followers, color=name)) + geom_point() 
p = p + geom_line()
p = p + ggtitle("New Facebook Followers per Month (millions)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13))
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("New Facebook Followers")
p = p + xlab("Month")
p = p + scale_x_continuous(breaks = c(2,3,4))
#p + scale_x_discrete(labels=c("2" = "Feb", "3" = "March", "4" = "April"))
ggsave(filename = sprintf("./%s_all_over_time.png", data_name) , plot=p, width=8, height=6)

data_name = "total_facebook_followers"
followers = read.csv("politician_facebook_totalfollowers.csv")
followers$politician = factor(followers$politician, levels = followers$politician[order(followers$followers, decreasing=TRUE)])
followers = followers[order(followers$followers, decreasing=TRUE),]
p = ggplot(followers, aes(x=politician, y=followers, fill=politician)) + geom_bar(stat="identity") 
p = p + ggtitle("Candidates Total Facebook Followers in April 2019 (millions)")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,face="bold"))
p = p + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=13), axis.title.x=element_blank())
p = p + theme(plot.title = element_text(size=18,face="bold"))
p = p + ylab("Total Facebook Followers")
p = p + xlab("Candidate")
ggsave(filename = sprintf("./%s_articles.png", data_name) , plot=p, width=8, height=6)
