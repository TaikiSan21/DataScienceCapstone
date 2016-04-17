# All functions created for my capstone project
#
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
#####################################################################
# Functions for using my dictionary, converting between words/numbers
#####################################################################
toNumbers <- function(x, d) {
      # x is ngram counts vector (colSums(dfm))
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
#####################################################################
# Functions for generating probabilities for uni/bi/tri
#####################################################################
KNUni <- function(uni, bi) {
      x <- rep(0, nrow(uni))
      temp <- summarise(group_by(bi, Word2), Follows=n())
      x[temp$Word2] <- temp$Follows
      x/nrow(bi)
}
KNBi <- function(w, uni, bi, d=.75) {
      # w should be one word
      C12 <- bi[Word1==w,]
      C1 <- uni[Word1==w, Count]
      N1S <- nrow(C12)
      x <- rep(0, nrow(uni))
      x[C12$Word2] <- C12$Count - d
      x[x<0] <- 0
      #y <- rep(0, nrow(uni)+1)
      #y[follows]
      y <- d*N1S*uni$Follows
      (x+y)/C1
}
KNTri <- function(w, uni, bi, tri, d=.75) {
      # w is a vector of words c(12, 4) 
      # Chaining == operations much faster than using &
      C12 <- bi[Word1==w[1],][Word2==w[2],Count]
      wTri <- tri[Word1==w[1],][Word2==w[2],]
      N12S <- nrow(wTri)
      #NS2S <- sum(tri$Word2==w[2])
      NS2S <- nrow(wTri[Word2==w[2],])
      # Bracket part is passed off
      PKNBi <- KNBiInt(w[2], uni, bi, d)/NS2S
      PKNBi <- PKNBi*d*N12S #modKN(tri, w, 3)
      # First part
      x <- rep(0, nrow(uni))
      x[wTri$Word3] <- wTri$Count #- wTri$D
      x <- x-d
      x[x<0] <- 0
      (x+PKNBi)/C12
}
KNBiInt <- function(w, uni, bi, d) {
      # w is word - start of bigram. For now needs to be number.
      # x is first half of equation
      # Outputs vector, position is word number
      # Access sorted probabilities by using
      # data.table(data, key='V2'), then tail of that
      biFilt <- bi[Word1==w,]
      N2S <- nrow(biFilt)
      C23 <- nrow(bi)
      x <- rep(0, nrow(uni))
      x[biFilt$Word2] <- biFilt$Follows
      x <- x-d
      x[x<0] <- 0
      # Second part
      y <- uni$Follows*d*N2S
      (x+y)
}
#####################################################################
# Functions for trying to use modified KN
#####################################################################
modKN <- function(data, w, n) {
      # Making the constant shit for modified KN
      # w=words, n=trigram/bigram
      n1 <- nrow(data[Count==1,])
      n2 <- nrow(data[Count==2,])
      n3 <- nrow(data[Count==3,])
      n4 <- nrow(data[Count==4,])
      D <- n1/(n1+2*n2)
      D1 <- 1-2*D*n2/n1
      D2 <- 2-3*D*n3/n2
      D3 <- 3-4*D*n4/n3
      if(n==3){
            temp <- data[Word1==w[1],][Word2==w[2],]
            
      }
      else {
            temp <- data[Word1==w,]
      }
      N1 <- nrow(temp[Count==1,])
      N2 <- nrow(temp[Count==2,])
      N3 <- nrow(temp) - N1 - N2
      D1*N1 + D2*N2 + D3*N3
}
# Creating the D1,2,3 for modified in column D. 0 for 0.
DMaker <- function(data){
      n1 <- nrow(data[Count==1,])
      n2 <- nrow(data[Count==2,])
      n3 <- nrow(data[Count==3,])
      n4 <- nrow(data[Count==4,])
      D <- n1/(n1+2*n2)
      D1 <- 1-2*D*n2/n1
      D2 <- 2-3*D*n3/n2
      D3 <- 3-4*D*n4/n3
      data$D <- D3
      data[Count==1,]$D <- D1
      data[Count==2,]$D <- D2
      data[Count==0,]$D <- 0
      data
}
#####################################################################
# The prediction function
#####################################################################
KNPredict <- function(s, dict, uni, bi, tri, split=' ') {
      sNum <- dict[unlist(strsplit(s, split))]
      revNum <- rev(sNum)
      if(nrow(tri[Word1==revNum[2],][Word2==revNum[1],]) > 0){
            KNPreds <- KNTri(c(revNum[2], revNum[1]), uni, bi, tri)
            print(3)
      }
      else if(nrow(bi[Word1==revNum[1],]) > 0){
            KNPreds <- KNBi(revNum[1], uni, bi)
            print(2)
      }
      else KNPreds <- uni$Follows
      # Getting top 5 probabilities
      KNPreds <- which(KNPreds >= -sort(-KNPreds, partial=3)[3])
      names(dict[KNPreds])
}