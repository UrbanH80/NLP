### load packages
library(ngram)
library(caret)
library(stringr)
library(dplyr)

### read in data
path <- getwd()
filepath <- paste(path,"/en_US.twitter.txt", sep = "")
linetwitter <- readLines(filepath)

path <- getwd()
filepath <- paste(path,"/en_US.blogs.txt", sep = "")
lineblogs <- readLines(filepath)

path <- getwd()
filepath <- paste(path,"/en_US.news.txt", sep = "")
news.con <- file(filepath,'rb')
linenews <- readLines(news.con) 
close(news.con)



### Exploratory Data Analysis
#check max line lengths
max(nchar(lineblogs))
max(nchar(linenews))
max(nchar(linetwitter))

#check avg line lengths
mean(nchar(lineblogs))
mean(nchar(linenews))
mean(nchar(linetwitter))

#grep examples
# grep("biostats", linetwitter)
#[1] 556772
# linetwitter[556772]
#[1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
# grep("A computer once beat me at chess, but it was no match for me at kickboxing", linetwitter)
#[1]  518959  835724 2283323



### Split into training set
## also preprocessing here

# sample 10% of data for training set.  10% for initial speed of model buidling
set.seed(3000)
sblogs <- sample(lineblogs, 0.1*length(lineblogs))
snews <- sample(linenews, 0.1*length(linenews))
stwit <- sample(linetwitter, 0.1*length(linetwitter))

#inTrain <- createDataPartition(y=faithful$waiting, p0.5, list=FALSE)
#trainFaith <- faithful[inTrain,]
#testFaith <- faithful[-inTraing,]
#dim(trainFaith)
#dim(testFaith)

#combine to one set
train <- c(sblogs,snews,stwit)
linecount <- length(train)

# using preprocess function from ngam packag to
        # make lowercase
        # remove punctuation
        # remove numbers
        # remove multiple and trailing spaces
processedtrain <- train
for(i in 1:linecount) {
        processedtrain[i] <- preprocess(train[i], 
                                        case = "lower", 
                                        remove.punct = TRUE,
                                        remove.numbers = TRUE, 
                                        fix.spacing = TRUE)
        }


# THIS DIDN'T WORK SO SWITCHING TO NLP PACKAGE
# use ngram function to tokenize each line in training set
#for(i in 1:426956) {
#        ng[i] <- ngram(processedtrain[i])
#}

#THIS DIDN'T WORK EITHER
# Using NLP libarary (ngrams function) to tokenize each element
# basically this is, for each element, recording the 1,2 and 3 word combinations in each element
# for example, for the unigram it's just recording each word in the element/string
#unigramtokenizer <- function(x)
#        unlist(lapply(ngrams(words(x),1), paste, collapse = " ", use.names = FALSE))
#bigramtokenizer <- function(x)
#        unlist(lapply(ngrams(words(x),2), paste, collapse = " ", use.names = FALSE))
#trigramtokenizer <- function(x)
#        unlist(lapply(ngrams(words(x),3), paste, collapse = " ", use.names = FALSE))


### Start creating tokens for each element of the processed data.
# basically this is, for each element, recording the 1,2 and 3 word combinations in each element
# for example, for the unigram it's just recording each word in the element/string
tokenizer <- function(x){
        tokens <- unlist(strsplit(x, " ")) # splitting on any space
                                           # note this successfully matches wordcount(processedtrain)
        tokens <- tokens[tokens != ""] # removing empty tokens, if any
}
tokens <- tokenizer(processedtrain) # this will be the unigrams
summary(tokens)

# get 2 word, 3 word, and 4 word combinations
tokens2 <- c(tokens[-1], ".")
tokens3 <- c(tokens2[-1], ".")
tokens4 <- c(tokens3[-1], ".")
# head(tokens)
# [1] "a"           "snapshot"    "of"          "the"         "global"      "therapeutic"
# head(tokens2)
# [1] "snapshot"    "of"          "the"         "global"      "therapeutic" "scenario"   
# head(tokens3)

# paste combinations together to get full list of unigrams, bigrams, trigrams and quadgrams
unigrams <- tokens
bigrams <- paste(tokens, tokens2)
trigrams <- paste(tokens, tokens2, tokens3)
quadgrams <- paste(tokens, tokens2, tokens3, tokens4)
# head(trigrams)
# [1] "a snapshot of"               "snapshot of the"             "of the global"          "the global therapeutic"      "global therapeutic scenario" "therapeutic scenario for"  

## NOT WORKING
#use grepl to remove string splits or else the sentences will run together in n-gram generation
#unigrams <- unigrams[!grepl("''split''", unigrams)] # not needed here
#bigrams <- bigrams[!grepl("''split''", bigrams)] 
#trigrams <- trigrams[!grepl("''split''", trigrams)] 

#sort n-grams on frequency
unigrams <- table(unigrams)
bigrams <- table(bigrams)
trigrams <- table(trigrams)
quadgrams <- table(quadgrams)
unigrams <- sort(unigrams, decreasing = TRUE)   #238,105
bigrams <- sort(bigrams, decreasing =TRUE)      #2,934,259
trigrams <- sort(trigrams, decreasing = TRUE)   #6,993,593
quadgrams <- sort(quadgrams, decreasing = TRUE) #9,157,241

ushort <- unigrams[1:10]
bshort <- bigrams[1:10]
tshort <- trigrams[1:10]
qshort <- quadgrams[1:10]


barchart(unigrams[1:20]) 
barchart(bigrams[1:20])
barchart(trigrams[1:20])
barchart(quadgrams[1:20])

hist(unigrams[1:10])
hist(bigrams[1:10])
hist(trigrams[1:10])
hist(quadrams[1:10])


### Calculating Probability of ngrams
# this will be used later in our model.  
#Word with highest probability will be output.
getLastWords <- function(string, words) {
        pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
        return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
        sub(" [a-z']+$", "", string)
}




kn_smooth <- function(ngrams, d) {
        
        n <- length(strsplit(names(ngrams[1]), " ")[[1]])
        
        #unigrams
        if(n==1){
                noFirst <- unigrams[getLastWords(names(bigrams), 1)]
                pContinuation <- table(names(noFirst))[names(unigrams)]/length(bigrams)
                return(pContinuation)
                }
                
        #gets the counts
         nMinusOne <- list(unigrams,bigrams,trigrams,quadgrams)[[n-1]]
         noLast <- nMinusOne[removeLastWord(names(ngrams))]
         noFirst <- nMinusOne[getLastWords(names(ngrams), n-1)]
         
        #calculations
        discounts <- ngrams - d
        discounts[discounts < 0 ] <- 0
        lambda <- d * table(names(noLast))[names(noLast)] / noLast
        if(n==2)
                pContinuation <- table(names(noFirst))[names(noFirst)] / length(ngrams)
        else
                pContinuation <- kn_smooth(noFirst, d)
                
        #output probabilities
        prob <- discounts / noLast + lambda * pContinuation / length(ngrams)
        return(prob)
}

#calculate the probability of each ngram
uprob <- kn_smooth(unigrams, .75)
bprob <- kn_smooth(bigrams, .75)
tprob <- kn_smooth(trigrams, .75)
qprob <- kn_smooth(quadgrams, .75)


#sort again after taking Kneser-Ney smoothing into account
u <- sort(uprob, decreasing = TRUE)   #238,105
b <- sort(bprob, decreasing =TRUE)      #2,934,259
t <- sort(tprob, decreasing = TRUE)   #6,993,593
q <- sort(qprob, decreasing = TRUE) #9,157,241

#looking at the bottom of the list.  KN has reduced the probability of these ngrams
#due to the frequency of the unigram in relation to the words preceding it.  In other
#words, unigrams in these groups have a variety of common words before them.
#For example, these have low prob: one of the best, one of the worst, one of the ballerinas, etc.
u2 <- sort(uprob, decreasing = FALSE)   #238,105
b2 <- sort(bprob, decreasing = FALSE)      #2,934,259
t2 <- sort(tprob, decreasing = FALSE)   #6,993,593
q2 <- sort(qprob, decreasing = FALSE) #9,157,241



########Building Prediction Model
# First we'll turn the probabilities into a table (data frame) so we can associate 
# the probabilities with each ngram
uDF <- data.frame("word" = (names(unigrams)), "KNprobability" = uprob, stringsAsFactors = FALSE)

bDF <- data.frame("firstwords" = removeLastWord(names(bigrams)),
                  "lastword" = getLastWords(names(bigrams), 1),
                  "KNprobability" = bprob, stringsAsFactors = FALSE)

tDF <- data.frame("firstwords" = removeLastWord(names(trigrams)),
                  "lastword" = getLastWords(names(trigrams), 1),
                  "KNprobability" = tprob, stringsAsFactors = FALSE)

qDF <- data.frame("firstwords" = removeLastWord(names(quadgrams)),
                  "lastwords" = getLastWords(names(quadgrams), 1),
                  "KNprobability" = qprob, stringsAsFactors = FALSE)


#high-grade probabilities for faster runtime
uDF <- arrange(uDF, desc(KNprobability.Freq)) 

bDF <- bDF %>% arrange(desc(KNprobability.Freq)) %>% 
                filter(KNprobability.Freq > .001)
tDF <- tDF %>% arrange(desc(KNprobability.Freq)) %>% 
                filter(KNprobability.Freq > .001)
qDF <- qDF %>% arrange(desc(KNprobability.Freq)) %>% 
                filter(KNprobability.Freq > .001)


#####Build prediction model
predict <- function(input){
        input <- preprocess(input, 
                                case = "lower", 
                                remove.punct = TRUE,
                                remove.numbers = TRUE, 
                                fix.spacing = TRUE)
        n <- length(strsplit(input, " ")[[1]])
        prediction <- c()
        if(n>3 && length(prediction)<3)
                prediction <- c(prediction, filter(qDF, getLastWords(input, 3) == firstwords)$lastword)
        if(n>2 && length(prediction)<3)
                prediction <- c(prediction, filter(tDF, getLastWords(input, 2) == firstwords)$lastword)
        if(n>1 && length(prediction)<3)
                prediction <- c(prediction, filter(bDF, getLastWords(input, 1) == firstwords)$lastword)
        if(length(prediction) < 3)
                prediction <- c(prediction, uDF$Words)
        
        return(unique(prediction)[1:3])
}


########## Calculate success from validation model

Donlp <- function(n, limit) {
        
        ngrams <- list(bprob, tprob, qprob)[[n-1]]
        
        nlp <- ngrams[getLastWords(quadgrams[1:10000], n)]
        names(nlp) <- v.quadgrams[1:10000]
        
        if(n >3) nlp[is.na(nlp) | nlp < limit] <- tprob[getLastWords(names(nlp[is.na(nlp) | nlp < limit]), 3)]
        if(n >2) nlp[is.na(nlp) | nlp < limit] <- bprob[getLastWords(names(nlp[is.na(nlp) | nlp < limit]), 2)]
        if(n >1) nlp[is.na(nlp) | nlp < limit] <- uprob[getLastWords(names(nlp[is.na(nlp) | nlp < limit]), 1)]
        
        return(nlp)
}

nlp <- Donlp(3, .001)





#Size and runtime reminders
#object.size(): this function reports the number of bytes that an R object occupies in memory
#Rprof(): this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.
#gc(): this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.
#There will likely be a tradeoff that you have to make in between size and runtime. For example, an algorithm that requires a lot of memory, may run faster, while a slower algorithm may require less memory. You will have to find the right balance between the two in order to provide a good experience to the user.
