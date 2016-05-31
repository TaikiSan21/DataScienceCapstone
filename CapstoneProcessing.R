# This is my data import and processing file for 
# the Coursera DS Specialization capstone project
# Sort of stream-of-consciousness, serves as a
# history of things I tried and how I ended up with
# my cleaned dataset. Functions used found in
# CapstoneFunctions.R
# 
library(tm)
library(dplyr)
library(SnowballC)
library(quanteda)
library(ggplot2)
library(gridExtra)
library(data.table)
setwd('~/R Projects/DataScienceCapstone')
source('CapstoneFunctions.R')

# US news has 1010242, 2206918 after
fraction <- .2
con <- file('en_US.news.txt', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
wholeDataVec <- readLines(con)
close(con)
smallDataVec <- sample(wholeDataVec, length(wholeDataVec)*fraction)
con <- file('en_US.twitter.txt', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
wholeDataVec <- readLines(con)
close(con)
smallDataVec <- append(smallDataVec, sample(wholeDataVec, length(wholeDataVec)*fraction))
con <- file('en_US.blogs.txt', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
# wholeDataVec <- readLines(con,10)
close(con)
smallDataVec <- append(smallDataVec, sample(wholeDataVec, length(wholeDataVec)*fraction))
rm(wholeDataVec)

# testdat <- readLines(con)
length(smallDataVec)
smallDataVec <- regexCleaner(smallDataVec)
length(smallDataVec)
#smallDataVec <- sapply(strsplit(smallDataVec, ' '), function(x) paste(wordStem(x), collapse=' '))
# Lower case and remove single words - things without spaces
smallDataVec <- toLower(smallDataVec)
smallDataVec <- smallDataVec[grep('\\s', smallDataVec)]
lessWords <- smallDataVec
rm(smallDataVec)
# Could create a function that creates list with ngrams, dfm, graph can access by result$graph

# words <- tokenize(smallDataVec, ngrams=1)
# words <- removeFeatures(words, profanity)
# sumWords <- colSums(dfm(words))
# topWords <- names(sumWords[sumWords > 4])
# 
# lessWords <- sapply(words, function(x) x[x %in% topWords])
# lessWords <- sapply(lessWords, function(x) paste(x, sep='', collapse=' '))

######################################
# MODEL CREATION STARTS HERE
######################################
# For each ngram we want just the count. 
# Size of matrix we will have. Obviously not all unique, but ngrams will match
# 
# Train test split and writing to file
con <- file('10percentall.txt','r', encoding='UTF-8')
lessWords <- readLines(con)
close(con)
train <- sample(1:length(lessWords), .8*length(lessWords))
trainWords <- lessWords[train]
testWords <- lessWords[-train]
rm(lessWords)
con <- file('10percentTrain.txt', encoding='UTF-8')
writeLines(trainWords, con)
close(con)
con <- file('10percentTest.txt', encoding='UTF-8')
writeLines(testWords, con)
close(con)

lessWords <- trainWords
rm(trainWords)

lessWords <- iconv(lessWords, 'UTF-8', 'ASCII')
# Making ngrams
unigrams <- tokenize(t, ngrams=1)
Nsize <- length(unlist(unigrams))
unisums <- colSums(dfm(unigrams))
rm(unigrams)
dict <- 1:(length(unisums)+1)
names(dict) <- c('.',names(unisums))#, 'xxxxx')
unimat <- toNumbers(unisums, dict)
unigrams <- data.table(unimat)
names(unigrams) <- c('Word1', 'Count')
unigrams <- rbind(data.table(Word1=1, Count=0, Follows=0), unigrams)
unigrams <- rbind(unigrams, data.table(Word1=235297, Count=0, Follows=0))

rm(unimat,unisums)
# To match size, append . to the start. Storing as numbers is 60-70% less
bisums <- colSums(dfm(tokenize(sapply(t, function(x) paste('.', x)), ngrams=2)))
bimat <- toNumbers(bisums, dict)
bigrams <- data.table(bimat)
names(bigrams) <- c('Word1', 'Word2', 'Count')
rm(bisums, bimat)
trisums <- colSums(dfm(tokenize(sapply(t, function(x) paste('. .', x)), ngrams=3)))
trimat <- toNumbers(trisums, dict)
trigrams <- data.table(trimat)
names(trigrams) <- c('Word1', 'Word2', 'Word3', 'Count')
rm(trisums, trimat)
# quadsums <- colSums(dfm(tokenize(sapply(lessWords, function(x) paste('. . .', x)), ngrams=4)))
# quadmat <- toNumbers(quadsums, dict)
# quaddat <- data.frame(quadmat)
# names(quaddat) <- c('Word1', 'Word2', 'Word3', 'Word4', 'Count')
# rm(quadsums, quadmat)

# Storing the follows
bigrams$Word1[is.na(bigrams$Word1)] <- 235297
bigrams$Word2[is.na(bigrams$Word2)] <- 1
trigrams$Word1[is.na(trigrams$Word1)] <- 235297
trigrams$Word2[is.na(trigrams$Word2)] <- 235297
trigrams$Word3[is.na(trigrams$Word3)] <- 1

unigrams$Follows <- KNUni(unigrams, bigrams)
unigrams <- rbind(data.table(Word1=1, Count=0, Follows=0), unigrams)
bigrams$Follows <- summarise(group_by(trigrams,Word2, Word3), Follows=n())$Follows
#########################
# Saving Data
write.csv(unigrams, 'unigrams.csv')
write.csv(bigrams, 'bigrams.csv')
write.csv(trigrams, 'trigrams.csv')
# write.csv(quaddat, 'quadgrams.csv')
saveRDS(dict,'dict.rds') # readRDS('dict.rds')
uniFollows <-summarise(group_by(bigrams, Word2), Follows=n())  
biFollows <- summarise(group_by(trigrams, Word2, Word3), FOllows=n())
#########################

# From here on misc. stuff, testing, profiling code to try and speed it up.

# First column is index for some reason

unigrams <- data.table(read.csv('unigrams.csv')[,c(2,3,4)])
bigrams <- data.table(read.csv('bigrams.csv')[,c(2,3,4,5)])
trigrams <- data.table(read.csv('trigrams.csv')[,c(2,3,4,5)])
dict <- readRDS('dict.rds')



####################
# Trying modified KN
####################
# Problem: Need the D values later, don't want to store so many
# follows for memory size. Might have to dump speed =(
# Not sure if I want to use DMaker before or inside.. BAH.
# Maybe just have it spit it out D instead of assign to DT.
bigrams <- DMaker(bigrams)
trigrams <- DMaker(trigrams)

con <- file('10percentTest.txt','r', encoding='UTF-8')
testWords <- readLines(con)
close(con)
triTest <- colSums(dfm(tokenize(sapply(testWords, function(x) paste('. .', x)), ngrams=3)))
triTest <- toNumbers(triTest, dict)
triTest <- data.table(triTest)
names(triTest) <- c('Word1', 'Word2', 'Word3', 'Count')


# Profiling to speed things up
Rprof()
for(i in 1:50){
t <- KNTri(c(12,4),unigrams,bigrams,trigrams)
}
Rprof(NULL)
summaryRprof()

Rprof()
for(i in 1:50){
      temp <- KNBi(4, unigrams, bigrams)
}
Rprof(NULL)
summaryRprof()
# bi$words, bi$count
# bi$prob <- 
t <- data.table(t)
Rprof()
for(i in 1:100){
      temp <- KNPredict('what comes after clam',dict, unigrams, bigrams, trigrams)
}
Rprof(NULL)
summaryRprof()

tenThousand <- sample(1:589332, 10000)
Rprof()
# ~ 4.5 hours to run on whole set
predictions <- unlist(apply(triTest[tenThousand,], 1, function(x) KNPredict(c(x[1], x[2]), dict, unigrams, bigrams, trigrams)))
Rprof(NULL)
summaryRprof()

predictText <- function(s, d, bi, tri, quad, split=' ') {
      sNum <- d[unlist(strsplit(s, split))]
      revNum <- rev(sNum)
      bpred <- arrange(bi[bi$Word1==revNum[1],], desc(Count))
      bpred$Count <- bpred$Count / sum(bpred$Count)
      tpred <- arrange(tri[tri$Word1==revNum[2] & tri$Word2 == revNum[1],], desc(Count))
      tpred$Count <- tpred$Count / sum(tpred$Count)
      qpred <- arrange(quad[quad$Word1==revNum[3] & quad$Word2==revNum[2] & quad$Word3==revNum[1],],
                       desc(Count))
      qpred$Count <- qpred$Count / sum(qpred$Count)
      result <- cbind(bpred[1:5,2:3],tpred[1:5,3:4], qpred[1:5,4:5])
      result[,-c(2,4,6)] <- sapply(result[,-c(2,4,6)], function(x) names(d[x]))
      result
      
}

# I wan to get DTM, then matrix <- inspect(DTM), then sort(colSums(matrix), descending=TRUE)
# to grab top 100 or whatever terms.
nGramTokenizer <- function(x, n) {
      # Takes a text document and changes from words to ngrams
      lapply(ngrams(words(x), n), paste, collapse=' ')
}

test[[1]][test[[1]] %in% c('i', 'was')]
new <- sapply(test, function(x) x[x %in% topWords])
sapply(new, function(x) paste(x, sep='', collapse=' '))