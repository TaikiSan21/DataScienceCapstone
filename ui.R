# Shiny UI for DS Specialization Capstone Project
library(shiny)
library(shinythemes)
shinyUI(fluidPage(
      tags$head(tags$style(
            HTML('
                 #sidebar {
                 background-color: #b3c6ff;
                  width: 420px;
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                  margin-top:10px;
                 }

                 #textbox {
                 border: 2px solid #003380;
                 border-radius: 2px;
                 font-family: Arial;
                  margin-left: 15px;
                  margin-right: 15px;
                  background-color: #c9d7ff}

                 #inbox {
                 border-left: 1px solid #003380;
                  height:100%;}

                #pred1 {
                 background-color: #c9d7ff;
                 border: 0px;
                 height: 24px;
                 font-family: Helvetica;
                 font-size:16px;
                  font-style: bold;
                 text-align: center;
                 margin-top: 2px;}
                 
                  #pred2 {
                 border: 0px;
                 height: 24px;
                 font-family: Helvetica;
                 font-size:16px;
                 font-style: bold;
                 text-align: center;
                 margin-top: 2px;}
                 
                  #pred3 {
                 background-color:#c9d7ff;
                 border: 0px;
                 height: 24px;
                 font-family: Helvetica;
                 font-size:16px;
                 font-style: bold;
                 text-align: center;
                 margin-top: 2px;}
                 
                 #head {
                 text-align: center;
                  vertical-align: bottom;
                 font-family: Helvetica;
                 font-size: 20px;
                 padding: 0px;')
      )),
      #theme=shinytheme('flatly'),
      title='Text Predict-o-matic',
      sidebarLayout(
            sidebarPanel(id='sidebar', 
                   fluidRow(
                         column(12,id='head', 'Text Predict-o-Matic')
                         ),
                   fluidRow(
                         column(12, 
                                textInput('words',label='', value='Type your text here'))
                   ),
                   fluidRow(id='textbox',
                         column(4, #id='test1',
                                #tags$style(type='text/css', '#pred1 {color:red; family: Helvetica;}'),
                                textOutput('pred1')),
                         column(4, id='inbox',
                                textOutput('pred2')),
                         column(4, id='inbox',
                                textOutput('pred3'))
                   ),
                   # fluidRow(
                   #       dataTableOutput('table')
                   # ),
                   br(),
                   fluidRow(
                         column(11, offset=.5,
                              actionButton('goButton', 'Click to predict the next word!'))
                   ),
                   br(),
                   fluidRow(
                         column(4, h5('Loading app...')),
                         column(4,
                                h5(textOutput('loading')))
                   )),
            mainPanel()
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