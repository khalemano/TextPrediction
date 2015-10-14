library(shiny)
shinyUI(
  fluidPage(
    titlePanel("Text Prediction"),
    sidebarLayout(
      sidebarPanel(
        textInput("textIn",label="Enter a phrase here:"),
        actionButton("enter","Enter"),
        actionButton("clear","Clear"),br(),br(),
        h4("Options:"),
        radioButtons("numSuggest","Number of Suggestions",
                     c("One"="one","Three"="three"),selected="three"),
        checkboxInput("showDetails","Show Model Output",TRUE),
        conditionalPanel(
          condition= "input.showDetails == true",
          tableOutput("predictions")
        ) 
      ),
      mainPanel(
        # Suggestion 1
        div(style="display:inline-block",actionButton("word1","Suggestion 1")),
        div(style="display:inline-block",textOutput("suggest1")),br(),br(),
        conditionalPanel(
          condition="input.numSuggest == 'three'",
        # Suggestion 2    
        div(style="display:inline-block",actionButton("word2","Suggestion 2")),
        div(style="display:inline-block",textOutput("suggest2")),br(),br(),
        # Suggestion 3   
        div(style="display:inline-block",actionButton("word3","Suggestion 3")),
        div(style="display:inline-block",textOutput("suggest3")),br(),br()
        ),

        p("This is an interface for a text prediction algorithm.
      Type a phrase into the input-box and click the enter button.
      Suggested words appear above. Click the button next to the
      suggestion to add it to your phrase. You can toggle the number of suggestions given
      in the options."),
        p("Click the 'Show Model Output' checkbox to see the raw output from the model.
      The model uses a trigram table and Kneser-Ney smoothing to determine word probabilities.
      The words with the highest probabilites are suggested.")
      )
    )
  )
)  

