# This is my data import and processing file for 
# the Coursera DS Specialization capstone project

### Simple way is to create input-output pairs - for Get the dog
### we would have y=the x = get, y=dog, x=the
### could expand to y = dog, x = get the
### limit expansion to 3 or 4 to simplify, create separate model for each case
library(tm)
library(dplyr)
library(SnowballC)
library(quanteda)
library(ggplot2)
library(gridExtra)
library(data.table)
source('CapstoneFunctions.R')
setwd('~/R Projects/DataScienceCapstone')

# US news has 1010242, 2206918 after
fraction <- .1
con <- file('en_US.news.txt', 'r', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
wholeDataVec <- readLines(con)
close(con)
smallDataVec <- sample(wholeDataVec, length(wholeDataVec)*fraction)
con <- file('en_US.twitter.txt', 'r', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
wholeDataVec <- readLines(con)
close(con)
smallDataVec <- append(smallDataVec, sample(wholeDataVec, length(wholeDataVec)*fraction))
con <- file('en_US.blogs.txt', 'r', encoding='UTF-8')
# con <- file('news_cleaned319.txt', 'r')
# wholeDataVec <- readLines(con,10)
close(con)
smallDataVec <- append(smallDataVec, sample(wholeDataVec, length(wholeDataVec)*fraction))
rm(wholeDataVec)

# I think I can use the quanteda package to sentence split..FML 2206918 by my splitting 
# testdat <- readLines(con)
length(smallDataVec)
smallDataVec <- regexCleaner(smallDataVec)
length(smallDataVec)
#smallDataVec <- sapply(strsplit(smallDataVec, ' '), function(x) paste(wordStem(x), collapse=' '))
# Lower case and remove single words - things without spaces
smallDataVec <- toLower(smallDataVec)
smallDataVec <- smallDataVec[grep('\\s', smallDataVec)]
profanity <- c('fuck', 'shit', 'damn', 'ass', 'cunt', 'bitch', 'dick')
# smallDataVec <- removeFeatures(smallDataVec, features=profanity)
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

# Making ngrams
unigrams <- tokenize(lessWords, ngrams=1)
Nsize <- length(unlist(unigrams))
unisums <- colSums(dfm(unigrams))
rm(unigrams)
dict <- 1:(length(unisums)+1)
unimat <- toNumbers(unisums, dict)
unidat <- data.table(unimat)
names(unidat) <- c('Word1', 'Count')
names(dict) <- c('.',names(unisums))
rm(unimat)
# To match size, append . to the start. Storing as numbers is 60-70% less
bisums <- colSums(dfm(tokenize(sapply(lessWords, function(x) paste('.', x)), ngrams=2)))
bimat <- toNumbers(bisums, dict)
bidat <- data.table(bimat)
names(bidat) <- c('Word1', 'Word2', 'Count')
rm(bisums, bimat)
trisums <- colSums(dfm(tokenize(sapply(lessWords, function(x) paste('. .', x)), ngrams=3)))
trimat <- toNumbers(trisums, dict)
tridat <- data.table(trimat)
names(tridat) <- c('Word1', 'Word2', 'Word3', 'Count')
rm(trisums, trimat)
# quadsums <- colSums(dfm(tokenize(sapply(lessWords, function(x) paste('. . .', x)), ngrams=4)))
# quadmat <- toNumbers(quadsums, dict)
# quaddat <- data.frame(quadmat)
# names(quaddat) <- c('Word1', 'Word2', 'Word3', 'Word4', 'Count')
# rm(quadsums, quadmat)

#########################
# Saving Data
write.csv(unigrams, 'unigrams.csv')
write.csv(bidat, 'bigrams.csv')
write.csv(tridat, 'trigrams.csv')
# write.csv(quaddat, 'quadgrams.csv')
saveRDS(dict,'dict.rds') # readRDS('dict.rds')
uniFollows <-summarise(group_by(bigrams, Word2), Follows=n())  
biFollows <- summarise(group_by(trigrams, Word2, Word3), FOllows=n())
#########################
# First column is index for some reason
unigrams <- data.table(read.csv('unigrams.csv')[,c(2,3)])
bigrams <- data.table(read.csv('bigrams.csv')[,c(2,3,4)])
trigrams <- data.table(read.csv('trigrams.csv')[,c(2,3,4,5)])
dict <- readRDS('dict.rds')

# Just store the damn follows
unigrams$Follows <- KNUni(unigrams, bigrams)
bigrams$Follows <- summarise(group_by(trigrams,Word2, Word3), Follows=n())$Follows


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

# Strategy for predicting function:
# Make trigrams for the test set, then just input each one.
# So input will be this_kind_of character
      
# if wanting to get top N results, tail(sort(x, partial=length(x)-(n-1)), n)
# nGrammer <- function(x, n) {
#       # Takes in character vector and n-gram number
#       # Returns list object with:
#             # data = tokenized n-gram data
#             # dfm = DFM of the data
#             # top = top 100 terms
#             # plot = ggplot object of top n-grams
#       data <- tokenize(x, ngrams=n)
#       dfm <- dfm(data)
# #       top <- topfeatures(dfm, 100)
# #       plot <- ggplot(data.frame(words=factor(names(top), levels=names(top)), count=top)[1:30,],
# #                      aes(x=words, y=count)) + geom_bar(stat='identity') + ggtitle(paste('Top ', n, '- grams')) +
# #             coord_flip()
#       list(data=data, dfm=dfm)#, top=top, plot=plot)
# }


# 
# con <- file('5ormore_7percent.txt')
# writeLines(lessWords, con)
# close(con)

# gramGrouper <- function(dfm) {
#       # Wants a DFM object of 2-grams or larger
#       # Splits the grams into last bit and first bit so
#       # we can group by first bit. Then the top frequency last
#       # bits are what we predict from.
#       # Is this faster if i dont insert directly into DF?
#       sumsDf <- data.frame(names=colnames(dfm), counts=colSums(dfm))
#       sumsDf$names <- as.character(sumsDf$names)
#       first <- sapply(sumsDf$names, function(x) gsub('(.*)_([^_]*)$', '\\1', x))
#       second <- sapply(sumsDf$names, function(x) gsub('(.*)_([^_]*)$', '\\2', x))
#       sumsDf$first <- first
#       sumsDf$second <- second
#       # sumsDf
#       # This gets the most frequent for every gram. Can start with this instead of giving options
#       sumsDf %>%
#             group_by(first) %>%
#             mutate(prob=counts/sum(counts))
# 
# }



# predictText <- function(s, b, t, q, p) {
#       # Predict from the string using bi-tri-quad
#       split <- strsplit(s, '_')[[1]]
#       bi <- split[4]
#       tri <- paste(split[3], bi,sep='_')
#       quad <- paste(split[2], tri, sep='_')
#       bpred <- arrange(b[b$first==bi,], desc(prob))[1:10,4:5]
#       tpred <- arrange(t[t$first==tri,], desc(prob))[1:10,4:5]
#       qpred <- arrange(q[q$first==quad,], desc(prob))[1:10,4:5]
#       ppred <- arrange(p[p$first==s,], desc(prob))[1:10,4:5]
#       cbind(bpred, tpred, qpred, ppred)
# }
# uni41662 bi387124 tri708903
# max(nchar(rawdat)) this shit takes so much memory
# myCorp <- VCorpus(VectorSource(testdat))
# rm(testdat)
# # Getting a text document object for use with tm
# # Instead stem with sapply(strsplit(vector, ' '), wordStem) with SnowballC
# 
# myCorp <- tm_map(myCorp, stemDocument)
# myCorp <- tm_map(myCorp, stripWhitespace)
# myCorp <- tm_map(myCorp, content_transformer(tolower))
# # Cant remove stopwords, they are used in sentences we need to predict
# # myCorp <- tm_map(myCorp, removeWords, stopwords('en'))
# 
# myCorp <- tm_map(myCorp, removeWords, profanity)

# I wan to get DTM, then matrix <- inspect(DTM), then sort(colSums(matrix), descending=TRUE)
# to grab top 100 or whatever terms.
nGramTokenizer <- function(x, n) {
      # Takes a text document and changes from words to ngrams
      lapply(ngrams(words(x), n), paste, collapse=' ')
}

test[[1]][test[[1]] %in% c('i', 'was')]
new <- sapply(test, function(x) x[x %in% topWords])
sapply(new, function(x) paste(x, sep='', collapse=' '))