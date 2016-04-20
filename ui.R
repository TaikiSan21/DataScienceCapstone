# Shiny UI for DS Specialization Capstone Project
library(shiny)

shinyUI(fluidPage(
      title='Text Predict-o-matic',
      fluidRow(
            column(4,
                   fluidRow(
                         column(6, h3('Text Inputs')),
                         column(6, h3('Text Predictions'))
                   ),
                   fluidRow(
                         column(6,
                                textInput('words', 'Type your text here!', value='')),
                         column(6,
                                fluidRow(
                                      column(4,
                                             textOutput('pred1')),
                                      column(4,
                                             textOutput('pred2')),
                                      column(4,
                                             textOutput('pred3'))
                                ))),
                   fluidRow(
                         actionButton('goButton', 'Click to predict!')
                   ),
                   fluidRow(
                         column(6, h4('Loading app...')),
                         column(6,
                                h4(textOutput('loading')))
                   ))
            # column(2, h2('Text Inputs'),
            #        fluidRow(
            #              textInput('words', 'Type your text here!', value = '')
            #        ),
            #        fluidRow(
            #              actionButton('goButton', 'Predict!')
            #        ),
            #        fluidRow(
            #              column(6, h4('Loading app...')),
            #              column(6,
            #                     h4(textOutput('loading')))
            #        )),
            # column(2, h2('Text Predictions'),
            #        fluidRow(
            #              column(4,
            #                     textOutput('pred1')),
            #              column(4,
            #                     textOutput('pred2')),
            #              column(4,
            #                     textOutput('pred3'))
            #        ))
      )
)
)