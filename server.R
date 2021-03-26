#
# This is the server logic of a Shiny web application to create a Next Word 
# Predictor done by Marc Arroyo as final product for the Capstone Project of the 
# Data Science Specialization by Johns Hopkins University in Coursera.
#
#

library(shiny)
library(data.table)
library(quanteda)

# Load n-grams files generated with 01_Create_Data_Files.R contained and source
# functions created to determine probabilities 02_Functions.R, both contained
# in main repository

# Load n-grams
if(!exists("unigram.list")){
    load("./files/unigram_list.Rda")
    load("./files/bigram_list.Rda")
    load("./files/trigram_list.Rda")
    load("./files/quadgram_list.Rda")
}

source("02_Functions.R", local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$predict <- renderTable(
        
        # tokenize input text
        if (input$str == "") {
            return("Input box is empty!")
        } else {
            words <- tokens(x = tolower(input$str),
                            what = "word", 
                            remove_punct = TRUE,
                            remove_symbols = TRUE,
                            remove_numbers = TRUE,
                            remove_url = FALSE,
                            remove_separators = TRUE,
                            verbose = TRUE)
            # Separe last three words or those available
            word1 <- rev(words[[1]])[3]
            word2 <- rev(words[[1]])[2]
            word3 <- rev(words[[1]])[1]
            
            # Search for next words depending on number of words filled in
            
            if (!is.na(word1)){
                pred <- nextword4gram(w1=word1, w2=word2, w3=word3)
            } else {
                if (!is.na(word2)){
                    pred <- nextword3gram(w1=word2, w2=word3)
                } else {
                    pred <- nextword2gram(w1=word3)
                }
            }
            
            return(pred)
            }    
    )
})

