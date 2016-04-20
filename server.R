library(shiny)
library(dplyr)
library(quanteda)
library(ggplot2)
library(gridExtra)
library(data.table)

source('CapstoneFunctions.R')

unigrams <- data.table(read.csv('unigrams.csv')[,c(2,3,4)])
bigrams <- data.table(read.csv('bigrams.csv')[,c(2,3,4,5)])
trigrams <- data.table(read.csv('trigrams.csv')[,c(2,3,4,5)])
dict <- readRDS('dict.rds')

shinyServer(
      function(input, output, session) {
            wordinput <- reactive({as.character(input$words)})
            predictions <- eventReactive(input$goButton, {
                  KNPredict(wordinput(), dict, unigrams, bigrams, trigrams)
            })
            output$pred1 <- renderText({predictions()[1]})
            output$pred2 <- renderText({predictions()[2]})
            output$pred3 <- renderText({predictions()[3]})
            output$loading <- renderText({'Complete!'})
            
      }
)