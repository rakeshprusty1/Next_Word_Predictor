require(digest)
require(stringi)
require(data.table)

################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################


# 01b. Get text from randomly selected tweets
################################################################################################

tweets <- readLines('data/tweets.txt', encoding = 'UTF-8')
#Take a random sample of 60 lines
tweets <- sample(tweets,60,replace = FALSE)

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"


# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
blogs <- readLines('data/blogs.txt', encoding = 'UTF-8')
#Take a random sample of 60 lines
blogs <- sample(blogs,60,replace = FALSE)
# verify checksum of loaded lines
digest(paste0(blogs, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2"



################################################################################################
#
# 02. Define the functions used for benchmarking
#
################################################################################################

# 02a. Pre-processing functions
################################################################################################

# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
    require(stringi)
    # append a space to the sentence (to make sure we always create one result with only the 
    # last word missing)
    sent <- paste0(line, ' ')

    sep <- stri_locate_all_regex(line, 
                                 pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                 omit_empty=T, 
                                 case_insensitive=T)[[1]]
    sapply(seq_len(nrow(sep)), 
           function(i) {
               c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                    nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
               )
               })
}, options=list(optimize=3))


# 02b. Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.
benchmark <- compiler::cmpfun(function(FUN, ..., sent.list, ext.output=T) {
    require(stringi)
    require(digest)
    require(data.table)
    
    result <- rbindlist(lapply(names(sent.list), 
           function(list.name) {  
               sentences <- sent.list[[list.name]]
               
               score <- 0
               max.score <-0
               hit.count.top3 <- 0
               hit.count.top1 <- 0
               total.count <- 0
               time <- system.time({
                   for (sent in sentences) {
                       split <- split.sentence(sent[1])
                       max.score <- max.score + ncol(split)*3
                       total.count <- total.count + ncol(split)
                       rank <- sapply(seq_len(ncol(split)),
                                      function(i) {
                                          min(which(FUN(split[1,i], ...)==split[2,i]),4)
                                      })
                       score <- score + sum(4-rank)
                       hit.count.top3 <- hit.count.top3 + sum(rank<4)
                       hit.count.top1 <- hit.count.top1 + sum(rank==1)
                   }
               })
               
               list('list.name' = list.name,
                    'line.count' = length(sentences),
                    'word.count' = sum(stri_count_words(sentences)),
                    'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                    'score' = score,
                    'max.score' = max.score,
                    'hit.count.top3' = hit.count.top3,
                    'hit.count.top1' = hit.count.top1,
                    'total.count' = total.count,
                    'total.runtime' = time[3]
               )               
           }), use.names=T)
    
    setkey(result, list.name)
    
    # The overall scores are calculated weighting each data set equally (independent of the 
    # number of lines in each dataset).
    overall.score.percent = 100 * result[,sum(score/max.score)/.N]
    overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
    overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
    average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
    number.of.predictions = result[,sum(total.count)]
    total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                       function(x) {
                                           object.size(get(x,
                                                           envir = .GlobalEnv,
                                                           inherits = FALSE))
                                           })))/(1024^2)
    cat(sprintf(paste0('Overall top-3 score:     %.2f %%\n',
                       'Overall top-1 precision: %.2f %%\n',
                       'Overall top-3 precision: %.2f %%\n',
                       'Average runtime:         %.2f msec\n',
                       'Number of predictions:   %d\n',
                       'Total memory used:       %.2f MB\n'),
                overall.score.percent,
                overall.precision.top1,
                overall.precision.top3,
                average.runtime,
                number.of.predictions,
                total.mem.used
                ))
    
    cat('\nDataset details\n')
    for (p.list.name in result$list.name) {
        res <- result[list(p.list.name)]
        cat(sprintf(paste0(' Dataset "%s" (%d lines, %d words, hash %s)\n',
                           '  Score: %.2f %%, Top-1 precision: %.2f %%, Top-3 precision: %.2f %%\n'
                           ),
                    p.list.name,
                    res$line.count,
                    res$word.count,
                    res$hash,
                    100 * res$score/res$max.score,
                    100 * res$hit.count.top1/res$total.count,
                    100 * res$hit.count.top3/res$total.count
        ))
    }
    
    if (ext.output==T) {
        packages <- sort(stri_replace_first_fixed(search()[stri_detect_regex(search(), 
                                                                             '^package:')], 
                                                  'package:', ''))
        
        cat(sprintf(paste0('\n\n%s, platform %s\n', 
                           'Attached non-base packages:   %s\n',
                           'Unattached non-base packages: %s'
                           ),
                   sessionInfo()$R.version$version.string,
                   sessionInfo()$platform,
                   paste0(sapply(sessionInfo()$otherPkgs, 
                                 function(pkg) {
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', '),
                   paste0(sapply(sessionInfo()$loadedOnly, 
                                 function(pkg) { 
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', ')
                   ))
    }
}, options=list(optimize =3))




################################################################################################
#
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
predict.baseline <- function(in_string, maxResults = 3) {
    #in_string <- toString(sent.list)
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
        unique(predictWord$next_word)
    }
    #predict words when only 1 word is entered
    if  (wordcount(input) == 1) {
        bi_result <- grepl(paste0("^",input," "),bi_df$ngrams)
        #If no match in bi gram,  unigram top words
        if (sum(bi_result) ==0) {
            names <- head(word(uni_df$ngrams,-2),maxResults)
            score <- head(uni_df$prop,maxResults)
            predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
            predictWord <- predictWord[order(predictWord$score,decreasing = T),]
            unique(predictWord$next_word)[1:maxResults]
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
            unique(predictWord$next_word)[1:maxResults]
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
                unique(predictWord$next_word)[1:maxResults]
            } else{
                names <- c(word(bi_result_df$ngrams,-2))
                score <- c(bi_result_df$score)
                if (wordcount(unique(names)) < maxResults){
                    names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                    score <- c(bi_result_df$score,0.4*0.4*uni_df$prop)
                }
                predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                (unique(predictWord$next_word)[1:maxResults])
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
            (unique(predictWord$next_word)[1:maxResults])
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
                    (unique(predictWord$next_word)[1:maxResults])
                } else{
                    names <- word(bi_result_df$ngrams,-2)
                    score <- bi_result_df$score
                    if (wordcount(unique(names)) < maxResults){
                        names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                        score <- c(bi_result_df$score,0.4*0.4*0.4*uni_df$prop)
                    }
                    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                    (unique(predictWord$next_word)[1:maxResults])
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
                (unique(predictWord$next_word)[1:maxResults])
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
            (unique(predictWord$next_word)[1:maxResults])
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
                        (unique(predictWord$next_word)[1:maxResults])
                    } else{
                        names <- word(bi_result_df$ngrams,-2)
                        score <- bi_result_df$score
                        if (wordcount(unique(names)) < maxResults){
                            names <- c(word(bi_result_df$ngrams,-2),word(uni_df$ngrams,-2))
                            score <- c(bi_result_df$score,0.4*0.4*0.4*0.4*uni_df$prop)
                        }
                        predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
                        predictWord <- predictWord[order(predictWord$score,decreasing = T),]
                        (unique(predictWord$next_word)[1:maxResults])
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
                    (unique(predictWord$next_word)[1:maxResults])
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
                (unique(predictWord$next_word)[1:maxResults])
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
            (unique(predictWord$next_word)[1:maxResults])
        }
    }
}
################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)
