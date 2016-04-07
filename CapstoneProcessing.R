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
setwd('~/Coursera/Capstone')
# directory has de_DE en_US ru_RU subfolders with the text files
#use tm_map(dataset, function, ...)
# stemDoc, stripWhitespace, content_transformer(tolower), removeWords(doc, vector of words to drop), stopwords(language='lowercase language') 
# also want to drop punctuation other than periods. I think each sentence should be
# a single data point, not just each line. 

regexCleaner <- function(charVector) {
      # drops anything not alphanumeric, a space, or a sentence ender. !?. Also keeping in -:, not sure if replace with space or not. Drops the non-ascii from above
      result <- gsub('[^[:alnum:][:space:]@!?:\\.\\-]', '', charVector)
      # emails and websites before U.S. and shit
      result <- gsub('(^|\\s)\\S*\\.(com|net|gov|org)', '\\1www', result, ignore.case=TRUE)
      result <- gsub('(^|\\s)[[:graph:]]*@[[:graph:]]*([[:punct:]]|$|\\s)', '\\1anemail\\2', result, ignore.case=TRUE)
      # We are going to split by !?.: later, need to drop the : and . that aren't end of thoughts. Start with ellipses
      result <- gsub('(\\s*\\.){2,}', '', result)
      # Dashes, either like-this or -- like this
      result <- gsub('(\\s*-)+', ' ', result)
      # Drop colon from time
      result <- gsub('([0-9]{1,2}):([0-9]{2})', '\\1\\2', result)
      ## BEFORE spacing after decimals
      # U.S. and websites These split before spacing decimals ### How tell if end of sentence?
      # Separating U.S.A or M.D.
      result <- gsub('([A-Za-z])\\.([A-Za-z])\\.([A-Za-z]?)\\.?', '\\1\\2\\3', result, ignore.case=TRUE)
      
      # Searching for [...].com/gov/net/org and swap with www. Stand-in for any website.
      
      # No. 5, etc.
      result <- gsub('no\\.\\s?([0-9]+)', 'no \\1', result, ignore.case=TRUE)
      # decimal numbers
      result <- gsub('([0-9]*)\\.([0-9]+)', '\\1\\2', result)
      
      # Spacing after every period. Consider using [A-Za-z] if i dont want to split decimals
      result <- gsub('\\.([A-Za-z])', '\\. \\1', result)
      # Changing any common abbrev.s
      # Titles
      result <- gsub('(^|\\s)(st|mr?s?|sens?|gov|dr|sr|jr|rep|lt|sgt|gen|cpt|capt?|vs|mt|prop|prof|tsp|tbsp|rev|dist|cpl|ave)\\.', '\\1\\2', result, ignore.case=TRUE)
      # Dates
      result <- gsub('(^|\\s)(mon|tues?|wed|thurs?|fri|sat|sun|jan|feb|mar|apr|may|jun|jul|aug|sept?|oct|nov|dec)\\.', '\\1\\2', result, ignore.case=TRUE)
      # Misc.
      result <- gsub('(^|\\s)(misc|e|w|n|s|etc)\\.', '\\1\\2', result, ignore.case=TRUE)
      result <- gsub('(^\\s|\\s$)', '', result, ignore.case=TRUE)
      result <- gsub('\\s{2,}', ' ', result)
      # With extra periods removed, split into sentences
      result <- strsplit(result, '[\\.!?:]')
      gsub('(^\\s|\\s$)', '', unlist(result), ignore.case=TRUE)
}
#####################
# READ IN 5ormore_7percent.txt
# Using 5 percent sample, top 20k words
# maybe this doesnt really work
#####################
# This is 5 or more instances of word, sampling at 7%. Can construct ngrams from here

# smallLocs <- smallLocs[8:length(smallLocs)]
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
write.csv(unidat, 'unigrams.csv')
write.csv(bidat, 'bigrams.csv')
write.csv(tridat, 'trigrams.csv')
# write.csv(quaddat, 'quadgrams.csv')
saveRDS(dict,'dict.rds') # readRDS('dict.rds')
wordFollows <-summarise(group_by(bidat, Word2), Follows=n())    
#########################

toNumbers <- function(x, d) {
      # x is ngram sums vector
      # d is our dictionary that is a named integer
      splitNames <- sapply(names(x), function(x) strsplit(x, '_'))
      cols <- length(splitNames[[1]])
      result <- matrix(0,length(x),cols)
      for(i in 1:cols){
            result[,i] <- d[sapply(splitNames, function(x) x[[i]])]
      }
      unname(cbind(result, x))
}

toWords <- function(x, d) {
      # x is a vector with numbers corresponding to words. Last is the count
      # d is our dictionary
      paste(names(d[x]), collapse=' ')
}

KNBi <- function(w, uni, bi, tri, follows, d=.75) {
      # w is word - start of bigram. For now needs to be number.
      # d is absolute cutoff
      # x is first half of equation
      # Outputs matrix, first column is word ID, second is prob.
      # Using a lot of structure of our data
      # Access sorted probabilities by using
      # data.table(data, key='V2'), then tail of that
      # or which(data >= -sort(-data, partial=5)[5]) gets indices(words)
      # when bi is data table 4x faster
      ####################################
      # Now used for intermediate trigram step
      ####################################
      # Create constants per sheet
      N2S <- sum(bi$Word1==w)
      C23 <- nrow(bi)
      # Create new follows object (2) in sheet
      follows2 <- summarise(group_by(tri[Word2==w,], Word3), Follows=n())
      x <- rep(0, nrow(uni)+1)
      x[follows2$Word3] <- follows2$Follows
      x <- x-d
      x[x<0] <- 0
      # Second part
      y <- rep(0, nrow(uni))
      # follows <- summarise(group_by(bi, Word2), Follows=n())
      y[follows$Word2] <- follows$Follows
      y[1] <- 0
      # Multiply by constant parts
      y <- y*d*N2S/C23
#       arrange(data.frame(Word= 1:length(y),Prob = (x+y)/uni[uni$Word1==w,]$Count),
#               desc(Prob))
      # Its uni[w-1] because uni does not have word 1 = . so each
      # word is not in its place, but shifted by one
      # Nevermind, just outputting as vector. Position is word number.
      (x+y)/uni[w-1,2]
}
Rprof()
for(i in 1:200){
t <- KNBi(4,unidat,bit,wordFollows)
}
Rprof(NULL)
summaryRprof()
# bi$words, bi$count
# bi$prob <- 
t <- data.table(t)
Rprof()
for(i in 1:100){
      temp <- t[Word1 == 5,]
}
Rprof(NULL)
summaryRprof()
KNTri <- function(w, uni, bi, tri, follows, d=.75) {
      # w is a vector of words c(12, 4) 
      # uni, bi, tri should all be data tables
      # follows is a stored table. For each word the # of words it follows
      # or the # of bigrams it ends. 
      # First we generate all of our constants, labeled on my paper
      C12 <- max(bi[Word1==w[1] & Word2==w[2], Count],1)
      N12S <- max(sum(tri$Word1==w[1] & tridat$Word2==w[2]),1)
      NS2S <- sum(tri$Word2==w[2])
      # Bracket part is passed off
      PKNBi <- KNBi(w[2], uni, bi, tri, follows, d)
      PKNBi <- PKNBi*d*N12S/NS2S
      # First part
      x <- rep(0, nrow(uni)+1)
      wTri <- tri[Word1==w[1] & Word2==w[2],]
      x[wTri$Word3] <- wTri$Count
      x <- x-d
      x[x<0] <- 0
      (x+PKNBi)/C12
}

KNPredict <- function(s, dict, uni, bi, tri, follows) {
      sNum <- dict[unlist(strsplit(s, ' '))]
      revNum <- rev(sNum)
      KNPreds <- KNTri(c(revNum[2], revNum[1]), uni, bi, tri, follows)
      KNPreds <- which(KNPreds >= -sort(-KNPreds, partial=5)[5])
      names(dict[KNPreds])
}
      
predictText <- function(s, d, bi, tri, quad) {
      sNum <- d[unlist(strsplit(s, ' '))]
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