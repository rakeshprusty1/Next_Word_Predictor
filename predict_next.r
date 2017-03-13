##Load Libraries:
library(ngram)
library(stringr)
library(stringi)
library(tm)
#Load datasets
uni_df <- readRDS("data/uni_df.rds")
bi_df <- readRDS("data/bi_df.rds")
tri_df <- readRDS("data/tri_df.rds")
tetra_df <- readRDS("data/tetra_df.rds")
penta_df <- readRDS("data/penta_df.rds")

predict_next <-function(in_string, maxResults = 5) {
    in_string_pro <- preprocess(in_string, case = "lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
    in_string_pro <- trimws(in_string_pro,"both")
    words <- tail(unlist(strsplit(in_string_pro," ")),4)
    input <- concatenate(words)
    input2 <- word(input,-1)
    input3 <- concatenate(word(input,-2:-1))
    input4 <- concatenate(word(input,-3:-1))
    
    #Predict words when no input or garbage input
    if  (wordcount(input) == 0) {
        names <- head(word(uni_df$ngrams,-2),maxResults)
        score <- head(uni_df$prop,maxResults)
        predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
        predictWord <- predictWord[order(predictWord$score,decreasing = T),]
        return(unique(predictWord$next_word)[1:maxResults])
    }
    #predict words when only 1 word is entered
    if  (wordcount(input) == 1) {
        bi_result <- grepl(paste0("^",input," "),bi_df$ngrams)
        #If no match in bi gram, return unigram top words
        if (sum(bi_result) ==0) {
            names <- head(word(uni_df$ngrams,-2),maxResults)
            score <- head(uni_df$prop,maxResults)
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            return(unique(predictWord$next_word)[1:maxResults])
        } else{
            bi_result_df<-bi_df[bi_result,]
            bi_result_df$score <- bi_result_df$freq/uni_df[grepl(paste0("^",input," "),uni_df$ngrams),]$freq
            names <- word(bi_result_df$ngrams,-2)
            score <- bi_result_df$score
            if (wordcount(unique(names)) < maxResults){
                names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                score <- c(bi_result_df$score,0.4*uni_df$prop)
            }
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            return(unique(predictWord$next_word)[1:maxResults])
        }
        
    }
    #Predict words when 2 words are entered
    if  (wordcount(input) == 2) {
        tri_result<-grepl(paste0("^",input," "),tri_df$ngrams)
        bi_result <- grepl(paste0("^",input2," "),bi_df$ngrams)
        bi_result_df<-bi_df[bi_result,]
        bi_result_df$score <- 0.4*(bi_result_df$freq/uni_df[grepl(paste0("^",input2," "),uni_df$ngrams),]$freq)
        #If no match in tri gram, search in bigrams
        if  (sum(tri_result) == 0) {
            
            if (sum(bi_result) ==0) {
                names <- head(word(uni_df$ngrams,-2),maxResults)
                score <- head(uni_df$prop,maxResults)
                predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                return(unique(predictWord$next_word)[1:maxResults])
            } else{
                names <- c(word(bi_result_df$ngrams,-2))
                score <- c(bi_result_df$score)
                if (wordcount(unique(names)) < maxResults){
                    names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                    score <- c(bi_result_df$score,0.4*0.4*uni_df$prop)
                }
                predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                return(unique(predictWord$next_word)[1:maxResults])
            }
            
        } else{
            tri_result_df<-tri_df[tri_result,]
            tri_result_df$score <- tri_result_df$freq/bi_df[grepl(paste0("^",input," "),bi_df$ngrams),]$freq
            names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
            score <- c(tri_result_df$score,bi_result_df$score)
            if (wordcount(unique(names)) < maxResults){
                names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                score <- c(tri_result_df$score,bi_result_df$score,0.4*0.4* uni_df$prop)
            }
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            return(unique(predictWord$next_word)[1:maxResults])
        }
        
    }
    #Predict words when 3 words are entered
    if  (wordcount(input) == 3) {
        tetra_result<-grepl(paste0("^",input," "),tetra_df$ngrams)
        tri_result <- grepl(paste0("^",input3," "),tri_df$ngrams)
        bi_result <- grepl(paste0("^",input2," "),bi_df$ngrams)
        
        tri_result_df<-tri_df[tri_result,]
        tri_result_df$score <- 0.4*(tri_result_df$freq/bi_df[grepl(paste0("^",input3," "),bi_df$ngrams),]$freq)
        bi_result_df<-bi_df[bi_result,]
        bi_result_df$score <- 0.4*0.4*(bi_result_df$freq/uni_df[grepl(paste0("^",input2," "),uni_df$ngrams),]$freq)
        
        #If no match in tetra gram, search in tri/bigrams
        if  (sum(tetra_result) == 0) {
            
            if  (sum(tri_result) == 0) {
                
                if (sum(bi_result) ==0) {
                    names <- head(word(uni_df$ngrams,-2),maxResults)
                    score <- head(uni_df$prop,maxResults)
                    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                    return(unique(predictWord$next_word)[1:maxResults])
                } else{
                    names <- word(bi_result_df$ngrams,-2)
                    score <- bi_result_df$score
                    if (wordcount(unique(names)) < maxResults){
                        names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                        score <- c(bi_result_df$score,0.4*0.4*0.4*uni_df$prop)
                    }
                    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                    return(unique(predictWord$next_word)[1:maxResults])
                }
            } else{
                tetra_result_df<-tetra_df[tetra_result,]
                tetra_result_df$score <- tetra_result_df$freq/tri_df[grepl(paste0("^",input," "),tri_df$ngrams),]$freq
                names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
                score <- c(tri_result_df$score,bi_result_df$score)
                if (wordcount(unique(names)) < maxResults){
                    names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                    score <- c(tri_result_df$score,bi_result_df$score,0.4*0.4*0.4*uni_df$prop)
                }
                predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                return(unique(predictWord$next_word)[1:maxResults])
            }
            
        } else{
            tetra_result_df<-tetra_df[tetra_result,]
            tetra_result_df$score <- tetra_result_df$freq/tri_df[grepl(paste0("^",input," "),tri_df$ngrams),]$freq
            names <- c(word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
            score <- c(tetra_result_df$score,tri_result_df$score,bi_result_df$score)
            if (wordcount(unique(names)) < maxResults){
                names <- c(word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                score <- c(tetra_result_df$score,tri_result_df$score,bi_result_df$score,0.4*0.4*0.4*uni_df$prop)
            }
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            return(unique(predictWord$next_word)[1:maxResults])
        }
        
    }
    #Predict words when 4 or more words are entered
    if  (wordcount(input) == 4) {
        penta_result<-grepl(paste0("^",input," "),penta_df$ngrams)
        tetra_result<-grepl(paste0("^",input4," "),tetra_df$ngrams)
        tri_result <- grepl(paste0("^",input3," "),tri_df$ngrams)
        bi_result <- grepl(paste0("^",input2," "),bi_df$ngrams)
        tetra_result_df<-tetra_df[tetra_result,]
        tetra_result_df$score <- 0.4*(tetra_result_df$freq/tri_df[grepl(paste0("^",input4," "),tri_df$ngrams),]$freq)
        tri_result_df<-tri_df[tri_result,]
        tri_result_df$score <- 0.4*0.4*(tri_result_df$freq/bi_df[grepl(paste0("^",input3," "),bi_df$ngrams),]$freq)
        bi_result_df<-bi_df[bi_result,]
        bi_result_df$score <- 0.4*0.4*0.4*(bi_result_df$freq/uni_df[grepl(paste0("^",input2," "),uni_df$ngrams),]$freq)
        
        #If no match in penta gram, search in tetra/tri/bigrams
        
        if (sum(penta_result) ==0){
            if (sum(tetra_result) ==0){
                if (sum(tri_result) ==0){
                    if (sum(bi_result) ==0){
                        names <- head(word(uni_df$ngrams,-2),maxResults)
                        score <- head(uni_df$prop,maxResults)
                        predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                        predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                        return(unique(predictWord$next_word)[1:maxResults])
                    } else{
                        names <- word(bi_result_df$ngrams,-2)
                        score <- bi_result_df$score
                        if (wordcount(unique(names)) < maxResults){
                            names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                            score <- c(bi_result_df$score,0.4*0.4*0.4*0.4*uni_df$prop)
                        }
                        predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                        predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                        return(unique(predictWord$next_word)[1:maxResults])
                    }
                    
                }else{
                    names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
                    score <- c(tri_result_df$score,bi_result_df$score)
                    if (wordcount(unique(names)) < maxResults){
                        names <- c(word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                        score <- c(tri_result_df$score,bi_result_df$score,0.4*0.4*0.4*0.4*uni_df$prop)
                    }
                    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                    return(unique(predictWord$next_word)[1:maxResults])
                }
                
            } else{
                names <- c(word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
                score <- c(tetra_result_df$score,tri_result_df$score,bi_result_df$score)
                if (wordcount(unique(names)) < maxResults){
                    names <- c(word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                    score <- c(tetra_result_df$score,tri_result_df$score,bi_result_df$score,0.4*0.4*0.4*0.4*uni_df$prop)
                }
                predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                return(unique(predictWord$next_word)[1:maxResults])
            }
            
        } else{
            penta_result_df<-penta_df[penta_result,]
            penta_result_df$score <- penta_result_df$freq/tetra_df[grepl(paste0("^",input," "),tetra_df$ngrams),]$freq
            
            names <- c(word(penta_result_df$ngrams,-2),word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2))
            score <- c(penta_result_df$score,tetra_result_df$score,tri_result_df$score,bi_result_df$score)
            if (wordcount(unique(names)) < maxResults){
                names <- c(word(penta_result_df$ngrams,-2),word(tetra_result_df$ngrams,-2),word(tri_result_df$ngrams,-2),word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                score <- c(penta_result_df$score,tetra_result_df$score,tri_result_df$score,bi_result_df$score,0.4*0.4*0.4*0.4*uni_df$prop)
            }
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            return(unique(predictWord$next_word)[1:maxResults])
        }
    }
}