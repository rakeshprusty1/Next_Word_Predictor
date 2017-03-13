#Load Librarries:
library(ngram)
library(stringr)
library(stringi)
library(tm)

#Reading Files:
twitter <- readLines("en_US.twitter.txt",encoding = "UTF-8")
news <- readLines("en_US.news.txt",encoding = "UTF-8")
blogs <- readLines("en_US.blogs.txt",encoding = "UTF-8")

#Sampling:
set.seed(1234)
twit_samp <- sample(twitter,15000,replace = FALSE)
blog_samp <- sample(blogs,15000,replace = FALSE)
news_samp <- sample(news,15000,replace = FALSE)

#Merge samples:
samp <- concatenate(blog_samp,news_samp,twit_samp,collapse = " ",rm.space = FALSE)

#Preprocessing:
##Remove numeric, spcl characters etc..
samp_pro <- gsub("[^a-zA-Z ]","",samp)
##Convert everything to lower caps 
samp_pro <- tolower(samp_pro)
##Remove profanity words
badtext <- readLines("profanity.txt")
samp_pro <- removeWords(samp_pro,badtext)
##Remove extra spaces
samp_pro <- stripWhitespace(samp_pro)

#Prepare N-grams:
unigram <- ngram(samp_pro,n=1,sep = " ")
bigram <- ngram(samp_pro,n=2,sep = " ")
trigram <- ngram(samp_pro,n=3,sep = " ")
tetragram <- ngram(samp_pro,n=4,sep = " ")
pentagram <- ngram(samp_pro,n=5,sep = " ")

#Convert ngrams to tables:
uni_df <- get.phrasetable(unigram)
bi_df <- get.phrasetable(bigram)
tri_df <- get.phrasetable(trigram)
tetra_df <- get.phrasetable(tetragram)
penta_df <- get.phrasetable(pentagram)

#Take subset of n-grams-df:(Optional, to improve efficiency)
uni_df <- subset(uni_df,uni_df$freq>1)
bi_df <- subset(bi_df,bi_df$freq>1)
tri_df <- subset(tri_df,tri_df$freq>1)
tetra_df <- subset(tetra_df,tetra_df$freq>1)
penta_df <- subset(penta_df,penta_df$freq>1)
    
