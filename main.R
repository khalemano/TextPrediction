
##################################
# Load workspace, libraries, utilities, and data
##################################
setwd("~/Coursera/CapstoneProject2")
library(hash)
source(file="utilities.R")

vocab = readLines("vocab.txt")
badwords = readLines("badwords.txt")

news = readLines("data/en_US.news.txt")
blogs = readLines("data/en_US.blogs.txt")
twitter = readLines("data/en_US.twitter.txt")


##################################
# Save data to quicken loading times
##################################

corpora = c(news,blogs,twitter)
rm(news,blogs,twitter)
save(corpora,file="data/en_US.corpora")

save(vocab,badwords,file="vocab_and_badwords")

##################################
# Build the model
##################################

load("model/vocab_and_badwords")
load("data/en_US.corpora")


trainLogicVector = get_train_logic_vector(corpora,perc=.002)
sum(trainLogicVector) # displays the number of selected lines
train = corpora[trainLogicVector]
remainder = corpora[!trainLogicVector]

unigramReverse = hash()
bigramReverse = hash()
trigram = hash()

generate_ngrams(train,vocab,unigramReverse,bigramReverse,trigram,progressPercent=5)

bigramPContinuation = create_p_continuation(bigramReverse)
unigramPContinuation = create_p_continuation(unigramReverse)

discountedUnigram = copy(unigramPContinuation)
add_lops_and_total(discountedUnigram)

discountedBigram = copy(bigramPContinuation)
add_lops_and_total(discountedBigram)

discountedTrigram = copy(trigram)
add_lops_and_total_to_trigram(discountedTrigram)
collapsedTrigram = collapse_trigram(discountedTrigram)

save(discountedUnigram,file="discountedUnigram")
save(discountedBigram,file="discountedBigram")
save(collapsedTrigram,file="collapsedTrigram")

#loading the saved ngrams
load(file="discountedUnigram")
load(file="discountedBigram")
load(file="collapsedTrigram")

##################################
# Testing the model
##################################

pkn_of_word(c("will","be","at"),discountedUnigram,discountedBigram,collapsedTrigram)
pkn_of_word(c("will","be","a"),discountedUnigram,discountedBigram,collapsedTrigram)

top_candidates(c("will","be"),badwords,discountedUnigram,discountedBigram,collapsedTrigram)

text_to_dataframe("i but now I will be",badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
text_to_dataframe("",badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)

