#final 

library(reader)
library(tm)
library(stringi)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(textmineR)
library(RWeka)
library(data.table)

twitter   <- readLines("C:/R/Course/Capstone/final/en_US/en_US.twitter.txt", encoding="UTF-8")
blogs    <- readLines("C:/R/Course/Capstone/final/en_US/en_US.blogs.txt")
news<-  readLines("C:/R/Course/Capstone/final/en_US/en_US.news.txt")

stri_stats_general(twitter)[1]
stri_stats_general(blogs)[1]
stri_stats_general(news)[1]
#totallines <- (stri_stats_general(twitter)[1]) + (stri_stats_general(blogs)[1]) + (stri_stats_general(news)[1])
#totallines

set.seed(1234)

twittersample <- sample(twitter, length(twitter)*.02)
newssample <- sample(news, length(news)*.02)
blogsample <- sample(blogs, length(blogs)*.02)

rm(blogs)
rm(news)
rm(twitter)


full_sample <- c(twittersample, newssample, blogsample)
full_sample <- iconv(full_sample, "UTF-8","ASCII", sub="")
#length(full_sample)

sample_corpus <- VCorpus(VectorSource(full_sample))

rm(twittersample)
rm(newssample)
rm(blogsample)

rm(full_sample)

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
#length(sample_corpus_clean)



rm(removeURL)
rm(removeSign)
rm(removeNum)
rm(removeapo)
rm(removeNonASCII)
rm(removerepeat)
rm(toLowerCase)
rm(removeSpace)

corp <- sample_corpus_clean


for(i in 1:6) {
  print(paste0("Extracting", " ", i, "-grams from corpus"))
  tokens <- function(x) unlist(lapply(ngrams(words(x), i), paste, collapse = " "), use.names = FALSE)
  tdm <- TermDocumentMatrix(corp, control = list(tokenize = tokens))
  tdmr <- sort(slam::row_sums(tdm, na.rm = T), decreasing=TRUE)
  tdmr.t <- data.table(token = names(tdmr), count = unname(tdmr)) 
  tdmr.t[,  paste0("w", seq(i)) := tstrsplit(token, " ", fixed=TRUE)]

  tdmr.t$token <- NULL
  
  
  print(paste0("In memory ", nrow(tdmr.t), " ", i, "-grams, using: "))
  print(object.size(tdmr.t), units='Mb')
  

  print( table(tdmr.t$count) )
  
  assign(paste0("ngram",i), tdmr.t)
}

ngram_stats <- data.frame(ngram = '', length = 0, count_min = 0, count_median = 0, count_mean = 0, count_max = 0, most_frequent_ngram='', mem = '')
for(i in 1:6) {

  s <- summary( eval(parse(text = paste0('ngram',i,'$count'))) )

  w <- eval(parse(text = paste0('ngram',i,'[1,seq(',i,')+1, with=F]')))
  most_frequent_ngram <- paste(unlist(w), sep=" ", collapse = " ")
  m <- paste(round(object.size(eval(parse(text = paste0('ngram',i))))/1024^2,1),'Mb')
  ngram_stats <- rbind(ngram_stats, 
                       data.frame(
                         ngram = paste0('ngram',i), 
                         length = nrow(eval(parse(text = paste0('ngram',i)))), 
                         count_min = s[1], 
                         count_median = s[3], 
                         count_mean = round(s[4],1),
                         count_max = s[6],
                         most_frequent_ngram = most_frequent_ngram,
                         mem = m
                       )
  )
}
rm(s); rm(w);rm(m);rm(most_frequent_ngram);rm(i);
ngram_stats <- ngram_stats[-1,]


rm(corp)
rm(tdmr.t)
rm(tdm)
rm(sample_corpus_clean)
rm(sample_corpus)
save.image("clean05.Rdata")

