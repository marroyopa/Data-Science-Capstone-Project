
library(dplyr)
library(stringi)
library(quanteda)
library(data.table)
library(stringr)
library(filesstrings)

Sys.setlocale("LC_ALL", "English")

if(!file.exists("./data/en_US.twitter.txt")){
        
        # File doesn't already exists, download the file
        url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        file <- "Coursera-SwiftKey.zip"
        download.file(url, file, method="curl")
        
        # Uncompress the file
        unzip(file)
        
        # Create data folder to store relevant files
        dir.create("./data")
        
        # Move English language files to the data folder and remove others
        file.move("./final/en_US/en_US.blogs.txt", "./data")
        file.move("./final/en_US/en_US.news.txt", "./data")
        file.move("./final/en_US/en_US.twitter.txt", "./data")
        
        unlink("Coursera-SwiftKey.zip", recursive = FALSE)
        unlink("./final", recursive = TRUE)
}

# Read and load files

TwitterFile <- readLines("./data/en_US.twitter.txt",encoding="UTF-8", 
                         skipNul = TRUE, warn = TRUE)

BlogsFile <- readLines("./data/en_US.blogs.txt",encoding="UTF-8", 
                       skipNul = TRUE, warn = TRUE)

NewsFile <- readLines("./data/en_US.news.txt",encoding="UTF-8", 
                      skipNul = TRUE, warn = TRUE)

# Files size

fsiztwt <- file.info("./data/en_US.twitter.txt")$size / 1024 ^ 2
fsizblo <- file.info("./data/en_US.blogs.txt")$size / 1024 ^ 2
fsiznew <- file.info("./data/en_US.news.txt")$size / 1024 ^ 2
fsiztot <- fsiztwt + fsizblo + fsiznew

# Number of lines of each file
tlintwt <- length(TwitterFile)
tlinblo <- length(BlogsFile)
tlinnew <- length(NewsFile)
tlintot <- tlintwt + tlinblo + tlinnew

# Number of words

twortwt <- sum(stri_count_words(TwitterFile))
tworblo <- sum(stri_count_words(BlogsFile))
twornew <- sum(stri_count_words(NewsFile))
twortot <- twortwt + tworblo + twornew

# Create a sample of 5% of lines using a binomial distribution

set.seed(1234)
TwitterSample <-  TwitterFile[rbinom(tlintwt, 1,  0.05)==1]
BlogsSample <-  BlogsFile[rbinom(tlinblo, 1,  0.05)==1]
NewsSample <-  NewsFile[rbinom(tlinnew, 1,  0.05)==1]

# Add the three samples in only one variable and free memory removing the
# three individual samples

TotSample <- c(BlogsSample, NewsSample, TwitterSample)
rm(BlogsSample, NewsSample, TwitterSample, BlogsFile, NewsFile, TwitterFile)

# Assure its ASCII conformity

TotSample <- iconv(TotSample,to = "ASCII",sub = "")

# Profanity Removal

profanity <- readLines("profanity_en.txt",encoding="UTF-8", 
                       skipNul = TRUE, warn = TRUE)

TotSample <- setdiff(x = TotSample, y = profanity)

# Create corpus with quanteda

sampleCorpus <- corpus(TotSample)

# Tokenization

tokensTot <- tokens(x = tolower(sampleCorpus),
                    what = "word", 
                    remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE,
                    remove_url = FALSE,
                    remove_separators = TRUE,
                    verbose = TRUE)

# Create a frequency matrix for words, bigrams, trigrams and quadgrams and
# save it as a data.frame in a file

# Create unigrams data.frame and save it as a file

toks_ngram <- tokens_ngrams(tokensTot, n = 1)
dfm_corp <- dfm(toks_ngram, stem = FALSE, verbose = TRUE)
features_ngram_dfm = textstat_frequency(dfm_corp) %>% arrange(desc(frequency))
unigram_list <- setDT(features_ngram_dfm)
save(unigram_list, file = "unigram_list.Rda")

# and will be loaded as load("unigram_list.Rda")

# Create bigrams data.frame and save it as a file breaking up both words

toks_ngram <- tokens_ngrams(tokensTot, n = 2)
dfm_corp <- dfm(toks_ngram, stem = FALSE, verbose = TRUE)
features_ngram_dfm = textstat_frequency(dfm_corp) %>% arrange(desc(frequency))
bigram_list <- setDT(features_ngram_dfm)
#To separe words of the bigram we will use stringr
bigram_list[, c("word1","word2") := data.table(str_split_fixed(feature,"_",2))]
save(bigram_list, file = "bigram_list.Rda")

# and will be loaded as load("bigram_list.Rda")

# Create trigrams data.frame and save it as a file breaking up the tree words

toks_ngram <- tokens_ngrams(tokensTot, n = 3)
dfm_corp <- dfm(toks_ngram, stem = FALSE, verbose = TRUE)
features_ngram_dfm = textstat_frequency(dfm_corp) %>% arrange(desc(frequency))
trigram_list <- setDT(features_ngram_dfm)
#To separe words of the trigram we will use stringr
trigram_list[, c("word1","word2", "word3") := data.table(str_split_fixed(feature,"_",3))]
save(trigram_list, file = "trigram_list.Rda")

# and will be loaded as load("trigram_list.Rda")

# Create quadgrams data.frame and save it as a file breaking up the four words

toks_ngram <- tokens_ngrams(tokensTot, n = 4)
dfm_corp <- dfm(toks_ngram, stem = FALSE, verbose = TRUE)
features_ngram_dfm = textstat_frequency(dfm_corp) %>% arrange(desc(frequency))
quadgram_list <- setDT(features_ngram_dfm)
#To separe words of the quadgram we will use stringr
quadgram_list[, c("word1","word2", "word3", "word4") := data.table(str_split_fixed(feature,"_",4))]
save(quadgram_list, file = "quadgram_list.Rda")

# and will be loaded as load("quadgram_list.Rda")

# And now will move the files to a new folder to have them regrouped

# Create data folder to store relevant files
dir.create("./files")

# Move English language files to the data folder and remove others
file.move("./unigram_list.Rda", "./files")
file.move("./bigram_list.Rda", "./files")
file.move("./trigram_list.Rda", "./files")
file.move("./quadgram_list.Rda", "./files")

# And eliminate original files for memory space saving
unlink("./data", recursive = TRUE)