#readlines first test script

#readLines("path/filename.txt")

#biostat <- grep("biostats", entwitter, value = T)

library(reader)
library(tm)
library(stringi)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(textmineR)
library(RWeka)

twitter   <- readLines("C:/R/Course/Capstone/final/en_US/en_US.twitter.txt", encoding="UTF-8")
blogs    <- readLines("C:/R/Course/Capstone/final/en_US/en_US.blogs.txt")
news<-  readLines("C:/R/Course/Capstone/final/en_US/en_US.news.txt")

stri_stats_general(twitter)[1]
stri_stats_general(blogs)[1]
stri_stats_general(news)[1]
totallines <- (stri_stats_general(twitter)[1]) + (stri_stats_general(blogs)[1]) + (stri_stats_general(news)[1])
totallines


set.seed(1234)

twittersample <- sample(twitter, length(twitter)*.01)
newssample <- sample(news, length(news)*.01)
blogsample <- sample(blogs, length(blogs)*.01)

full_sample <- c(twittersample, newssample, blogsample)
full_sample <- iconv(full_sample, "UTF-8","ASCII", sub="")
length(full_sample)

sample_corpus <- VCorpus(VectorSource(full_sample))

removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
removeSign <- function(x) gsub("[[:punct:]]","",x)
removeNum <- function(x) gsub("[[:digit:]]","",x)
removeapo <- function(x) gsub("'","",x)
removeNonASCII <- function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace <- function(x) gsub("\\s+"," ",x)

sample_corpus_clean <-tm_map(sample_corpus,content_transformer(removeapo))#remove apostrophe
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removeNum))#remove numbers
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removeURL)) #remove web url
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removeSign)) #remove number and punctuation except apostrophe
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removeNonASCII)) #remove non-ASCII
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(toLowerCase))# convert uppercase to lowercase
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removerepeat))# remove repeated alphabets in a words
sample_corpus_clean <-tm_map(sample_corpus_clean,content_transformer(removeSpace)) #remove multiple space
sample_corpus_clean <-tm_map(sample_corpus_clean,removeWords,stopwords("english"))
length(sample_corpus_clean)


cloud.gram1 <- wordcloud(sample_corpus_clean, max.words=50, random.order=TRUE, rot.per=.15, colors=brewer.pal(8, "Set2"))

#gram1
data1 <- TermDocumentMatrix(sample_corpus_clean)
matrix1 = as.data.frame((as.matrix(data1))) 

freq1 <- sort(rowSums(matrix1),decreasing=TRUE)
names1 <- data.frame(word = names(freq1),frequency=freq1)
plot <-names1[1:20,]

visual.gram1 <- ggplot(plot, aes(x = as.factor(word), y= frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("word")+
  ylab("Frequency")

#2-Gram


set.seed(1234)

twittersample2 <- sample(twitter, length(twitter)*.002)
newssample2 <- sample(news, length(news)*.002)
blogsample2 <- sample(blogs, length(blogs)*.002)

full_sample2 <- c(twittersample2, newssample2, blogsample2)
full_sample2 <- iconv(full_sample2, "UTF-8","ASCII", sub="")
length(full_sample2)

sample_corpus2 <- VCorpus(VectorSource(full_sample2))

removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
removeSign <- function(x) gsub("[[:punct:]]","",x)
removeNum <- function(x) gsub("[[:digit:]]","",x)
removeapo <- function(x) gsub("'","",x)
removeNonASCII <- function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace <- function(x) gsub("\\s+"," ",x)

sample_corpus_clean2 <-tm_map(sample_corpus2,content_transformer(removeapo))#remove apostrophe
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removeNum))#remove numbers
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removeURL)) #remove web url
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removeSign)) #remove number and punctuation except apostrophe
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removeNonASCII)) #remove non-ASCII
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(toLowerCase))# convert uppercase to lowercase
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removerepeat))# remove repeated alphabets in a words
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,content_transformer(removeSpace)) #remove multiple space
sample_corpus_clean2 <-tm_map(sample_corpus_clean2,removeWords,stopwords("english"))
length(sample_corpus_clean2)



gram2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

data2 <-TermDocumentMatrix(sample_corpus_clean2, control = list(tokenize = gram2))

matrix2 = as.data.frame((as.matrix( data2 )) )

freq2 <- sort(rowSums(matrix2),decreasing=TRUE)
names2 <- data.frame(word = names(freq2),frequency=freq2)
plot2 <-names2[1:20,]

visual.gram2 <- ggplot(plot2, aes(x = as.factor(word), y= frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("word2")+
  ylab("Frequency")

wordcloud(names2$word, names2$frequency, max.words=25, random.order=TRUE, rot.per=.15, colors=brewer.pal(8, "Set2"), scale=c(3, .3))


#3-Gram


set.seed(1234)

twittersample3 <- sample(twitter, length(twitter)*.002)
newssample3 <- sample(news, length(news)*.002)
blogsample3 <- sample(blogs, length(blogs)*.002)

full_sample3 <- c(twittersample3, newssample3, blogsample3)
full_sample3 <- iconv(full_sample3, "UTF-8","ASCII", sub="")
length(full_sample3)

sample_corpus3 <- VCorpus(VectorSource(full_sample3))

removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
removeSign <- function(x) gsub("[[:punct:]]","",x)
removeNum <- function(x) gsub("[[:digit:]]","",x)
removeapo <- function(x) gsub("'","",x)
removeNonASCII <- function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat <- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace <- function(x) gsub("\\s+"," ",x)

sample_corpus_clean3 <-tm_map(sample_corpus3,content_transformer(removeapo))#remove apostrophe
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removeNum))#remove numbers
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removeURL)) #remove web url
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removeSign)) #remove number and punctuation except apostrophe
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removeNonASCII)) #remove non-ASCII
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(toLowerCase))# convert uppercase to lowercase
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removerepeat))# remove repeated alphabets in a words
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,content_transformer(removeSpace)) #remove multiple space
sample_corpus_clean3 <-tm_map(sample_corpus_clean3,removeWords,stopwords("english"))
length(sample_corpus_clean3)



gram3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

data3 <-TermDocumentMatrix(sample_corpus_clean3, control = list(tokenize = gram3))

matrix3 = as.data.frame((as.matrix( data3 )) )

freq3 <- sort(rowSums(matrix3),decreasing=TRUE)
names3 <- data.frame(word = names(freq3),frequency=freq3)
plot3<-names3[1:20,]


ggplot(plot3, aes(x = as.factor(word), y= frequency)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("word3")+
  ylab("Frequency")

wordcloud(names3$word, names3$frequency, max.words=25, random.order=TRUE, rot.per=.15, colors=brewer.pal(8, "Set2"), scale=c(3, .3))


