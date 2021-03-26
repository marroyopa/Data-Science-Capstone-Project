# In this file we will select the 20 most likely words to continue the sentence
# usign those with more frequency in the corresponding N-Gram Table, and if we
# don't found 20, going to the N-1-Gram table.
# Then we will use Kneser-Ney smoothing to calculate the probability of those
# 20 candidates and select the 5 most probable to show
#
# The explanation for Kneser-Ney Smoothing can be found here:
# https://www.youtube.com/watch?v=cbAxvpBFyNU&list=PLLssT5z_DsK8HbD2sPcUIDfQ7zmBarMYv&index=19&t=46s
#
# And a practical guide of the recursive Kneser-Ney Formula for 4-Grams can be 
# found in the last page of following document:
# https://kola.opus.hbz-nrw.de/opus45-kola/frontdoor/deliver/index/docId/796/file/bachelor_thesis.pdf

# Function to predict next word using 4-grams table as starting point in case
# we have 3 or more words written by the user

nextword4gram <- function(w1, w2, w3){        
        
        # Subset bigrams with last word entered
        bigrams <- subset(bigram_list, word1 == w3, c(word1, word2, frequency))
        Nbis <- nrow(bigrams)
        Fullbigs <- nrow(bigram_list)
        # Subset trigrams with two last words entered
        trigrams <- subset(trigram_list, word1 == w2 & word2 == w3, 
                           c(word1, word2, word3, frequency))
        Ntris <- nrow(trigrams)
        # Subset quadgrams with two last words entered
        quadgrams <- subset(quadgram_list, word1 == w1 & word2 == w2 & 
                                    word3 == w3, c(word1, word2, word3, word4, 
                                                   frequency))
        Nquads <- nrow(quadgrams)

        if (Nquads > 20){
                candidate <- data.table(cand = head(quadgrams$word4, 20))
        } else  {
                candidate <- data.table(cand = head(quadgrams$word4, Nquads))
                Nlack <- 20-Nquads
                if (Ntris > Nlack){
                        candidate <- rbindlist(list(candidate,
                                             list(head(trigrams$word3, Nlack))))
                } else {
                        candidate <- rbindlist(list(candidate,
                                             list(head(trigrams$word3, Ntris))))
                        Nlack <- 20 - nrow(candidate)
                        if (Nbis > Nlack){
                                candidate <- rbindlist(list(candidate,
                                              list(head(bigrams$word2, Nlack))))
                        } else {
                                candidate <- rbindlist(list(candidate,
                                              list(head(bigrams$word2, Nbis))))
                                Nlack <- 20 - nrow(candidate)
                                candidate <- rbindlist(list(candidate,
                                        list(head(unigram_list$feature,Nlack))))
                        }
                } 
        }
        
        candidate <- unique(candidate)
        Ncand <- nrow(candidate)
        
        for (i in 1:Ncand) {
                
                w4 <- candidate$cand[i]

                # Build quadgrams variables for recursive Kneser-Ney function
        
                isquad <- TRUE
                cquad <- quadgrams$frequency[quadgrams$word1 == w1 & 
                                             quadgrams$word2 == w2 & 
                                             quadgrams$word3 == w3 & 
                                             quadgrams$word4 == w4]
                totquads <- sum(trigram_list$frequency[trigram_list$word1 == w1 & 
                                                       trigram_list$word2 == w2 &
                                                       trigram_list$word3 == w3]) 
                if(length(cquad) == 0){ 
                        isquad <- FALSE
                        Dq <- 0.000027
                } else {
                        if(cquad>1){
                                Dq <- 0.75
                        } else {
                                Dq <- 0.554
                        }
                }
        
                #Build trigrams variables for recursive Kneser-Ney function
        
                istri <- TRUE
                ctri <- trigrams$frequency[trigrams$word1 == w2 &
                                           trigrams$word2 == w3 &
                                           trigrams$word3 == w4]
                tottris <- sum(bigram_list$frequency[bigram_list$word1 == w2 & 
                                                     bigram_list$word2 == w3]) 
                if(length(ctri) == 0){
                        istri <- FALSE
                        Dt <- 0.000027
                } else {
                        if(ctri>1){
                                Dt <- 0.75
                        } else {
                                Dt <- 0.554
                        }
                }
        
                #Build bigrams variables for recursive Kneser-Ney function
        
                isbi <- TRUE
                cbi <- bigrams$frequency[bigrams$word1 == w3 &
                                         bigrams$word2 == w4]
                Zbis <- bigram_list[bigram_list$word2==w4, .N]
                totbis <- sum(unigram_list$frequency[unigram_list$feature == w3])
                if(length(cbi) == 0){
                        isbi <- FALSE
                        Db <- 0.000027
                } else {
                        if(cbi>1){
                        Db <- 0.75
                        } else {
                                Db <- 0.554
                        }
                }
        
                # Calculate probability in a recursive form
        
                if(isbi == TRUE) {
                        PKNb <- ((cbi-Db)/totbis) + ((Db*Nbis*Zbis)/(totbis*Fullbigs))
                        if (istri == TRUE){
                                PKNt <- ((ctri-Dt)/tottris) + (((Dt*Ntris)/tottris)*PKNb)
                                if (isquad == TRUE){
                                        PKN <- ((cquad-Dq)/totquads) + (((Dq*Nquads)/totquads)*PKNt)
                                } else {
                                        PKN <- ((Dq*Nquads)/totquads)*PKNt
                                }
                        } else {
                                PKN <- ((Dt*Ntris)/tottris)*PKNb
                        }
                } else {
                        PKN <- (Db*Nbis*Zbis)/(totbis*Fullbigs)
                }
                
        
        
                # Fill in the w4 Kneser-Ney probability table
        
                if (i == 1){
                        Probtab <- data.table(Word = w4,Prob = PKN)
                } else {
                        Probtab <- rbindlist(list(Probtab,list(w4,PKN)))
                }
        }
        Probtab <- setorder(Probtab, -Prob)
        return(head(Probtab, 5))
}

# Function to predict next word using 3-grams table as starting point in case
# we have 2 words written by the user

nextword3gram <- function(w1, w2){        
        
        # Subset bigrams with last word entered
        bigrams <- subset(bigram_list, word1 == w2, c(word1, word2, frequency))
        Nbis <- nrow(bigrams)
        Fullbigs <- nrow(bigram_list)
        # Subset trigrams with two last words entered
        trigrams <- subset(trigram_list, word1 == w1 & word2 == w2, 
                           c(word1, word2, word3, frequency))
        Ntris <- nrow(trigrams)
        
        if (Ntris > 20){
                candidate <- data.table(cand = head(trigrams$word3, 20))
        } else  {
                candidate <- data.table(cand = head(trigrams$word3, Ntris))
                Nlack <- 20-Ntris
                if (Nbis > Nlack){
                        candidate <- rbindlist(list(candidate,
                                                    list(head(bigrams$word2, Nlack))))
                } else {
                        candidate <- rbindlist(list(candidate,
                                                    list(head(bigrams$word2, Nbis))))
                        Nlack <- 20 - nrow(candidate)
                        candidate <- rbindlist(list(candidate,
                                                    list(head(unigram_list$feature,Nlack))))
                }
        }
        
        candidate <- unique(candidate)
        Ncand <- nrow(candidate)
        
        for (i in 1:Ncand) {
                
                w3 <- candidate$cand[i]
                
                #Build trigrams variables for recursive Kneser-Ney function
                
                istri <- TRUE
                ctri <- trigrams$frequency[trigrams$word1 == w1 &
                                                   trigrams$word2 == w2 &
                                                   trigrams$word3 == w3]
                tottris <- sum(bigram_list$frequency[bigram_list$word1 == w1 & 
                                                             bigram_list$word2 == w2]) 
                if(length(ctri) == 0){
                        istri <- FALSE
                        Dt <- 0.000027
                } else {
                        if(ctri>1){
                                Dt <- 0.75
                        } else {
                                Dt <- 0.554
                        }
                }
                
                #Build bigrams variables for recursive Kneser-Ney function
                
                isbi <- TRUE
                cbi <- bigrams$frequency[bigrams$word1 == w2 &
                                                 bigrams$word2 == w3]
                Zbis <- bigram_list[bigram_list$word2==w3, .N]
                totbis <- sum(unigram_list$frequency[unigram_list$feature == w2])
                if(length(cbi) == 0){
                        isbi <- FALSE
                        Db <- 0.000027
                } else {
                        if(cbi>1){
                                Db <- 0.75
                        } else {
                                Db <- 0.554
                        }
                }
                
                # Calculate probability in a recursive form
                
                if(isbi == TRUE) {
                        PKNb <- ((cbi-Db)/totbis) + ((Db*Nbis*Zbis)/(totbis*Fullbigs))
                        if (istri == TRUE){
                                PKN <- ((ctri-Dt)/tottris) + (((Dt*Ntris)/tottris)*PKNb)
                        } else {
                                PKN <- ((Dt*Ntris)/tottris)*PKNb
                        }
                } else {
                        PKN <- (Db*Nbis*Zbis)/(totbis*Fullbigs)
                }
                
                
                # Fill in the w3 Kneser-Ney probability table
                
                if (i == 1){
                        Probtab <- data.table(Word = w3,Prob = PKN)
                } else {
                        Probtab <- rbindlist(list(Probtab,list(w3,PKN)))
                }
        }
        Probtab <- setorder(Probtab, -Prob)
        return(head(Probtab, 5))
}

# Function to predict next word using 2-grams table as starting point in case
# we have only one word written by the user

nextword2gram <- function(w1){        
        
        # Subset bigrams with last word entered
        bigrams <- subset(bigram_list, word1 == w1, c(word1, word2, frequency))
        Nbis <- nrow(bigrams)
        Fullbigs <- nrow(bigram_list)
        
        if (Nbis > 20){
                candidate <- data.table(cand = head(bigrams$word2, 20))
        } else  {
                candidate <- data.table(cand = head(bigrams$word2, Nbis))
                Nlack <- 20-Nbis
                candidate <- rbindlist(list(candidate,
                                            list(head(unigram_list$feature,Nlack))))
        }
        
        candidate <- unique(candidate)
        Ncand <- nrow(candidate)
        
        for (i in 1:Ncand) {
                
                w2 <- candidate$cand[i]
                
                #Build bigrams variables for recursive Kneser-Ney function
                
                isbi <- TRUE
                cbi <- bigrams$frequency[bigrams$word1 == w1 &
                                                 bigrams$word2 == w2]
                Zbis <- bigram_list[bigram_list$word2==w2, .N]
                totbis <- sum(unigram_list$frequency[unigram_list$feature == w1])
                if(length(cbi) == 0){
                        isbi <- FALSE
                        Db <- 0.000027
                } else {
                        if(cbi>1){
                                Db <- 0.75
                        } else {
                                Db <- 0.554
                        }
                }
                
                # Calculate probability in a recursive form
                
                if(isbi == TRUE) {
                        PKN <- ((cbi-Db)/totbis) + ((Db*Nbis*Zbis)/(totbis*Fullbigs))
                } else {
                        PKN <- (Db*Nbis*Zbis)/(totbis*Fullbigs)
                }
                
                
                # Fill in the w3 Kneser-Ney probability table
                
                if (i == 1){
                        Probtab <- data.table(Word = w2,Prob = PKN)
                } else {
                        Probtab <- rbindlist(list(Probtab,list(w2,PKN)))
                }
        }
        Probtab <- setorder(Probtab, -Prob)
        return(head(Probtab, 5))
}