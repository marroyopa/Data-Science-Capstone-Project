#
# This is the user-interface definition of a Shiny web application to create a 
# Next Word Predictor done by Marc Arroyo as final product for the Capstone 
# Project of the Data Science Specialization by Johns Hopkins University in 
# Coursera.
#

library(shiny)

# Define UI for application that predicts next word

shinyUI(fluidPage(
    titlePanel("Predict Next Word"),
    sidebarLayout(
        sidebarPanel(
            helpText("Write in some text, and next word will be predicted"),
            textInput("str",label = h3("Enter some Text:"),value = "")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Main",
                                 h2("Next word best guess:"),
                                 h4(tableOutput("predict"))
                        ),
                        tabPanel("Instructions", br(),
                                 h2("Instructions to use the App"),
                                 h3("Objectives"),
                                 p("This app loads four files, from 4-grams to
                                   unigrams, generated offline from a Corpus 
                                   provided by SwiftKey, corporate partner on 
                                   the Project Capstone for the Data Science 
                                   Specialization of Johns Hopkins University on
                                   Coursera."),
                                 p("With this n-grams the app reads the text you 
                                   write in the Input box, and appliying Kneser-
                                   Ney Smoothing recursive formula calculates 
                                   the probability for next word of a set of 20, 
                                   and shows the five words with highest 
                                   probability"),
                                 h3("How to use"),
                                 p("The only thing you have to do is write your 
                                   text on the Input text box and the app will
                                   return the best 5 options with their 
                                   calculated probability."),
                                 h3("Warning"),
                                 p("Some times the calculated probability is 
                                   really low and appears as NA."))
                        
            )
        )
    )
))