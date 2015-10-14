library(shiny)
library(hash)
source(file="utilities.R")

shinyServer(
  function(input,output,clientData,session){
    load(file="model/vocab_and_badwords")
    load(file="model/discountedUnigram")
    load(file="model/discountedBigram")
    load(file="model/collapsedTrigram")
    
    #sets session variables
    v = reactiveValues(df=data.frame())
    
    observe({
      input$enter
      isolate({
        v$df = text_to_dataframe(input$textIn,badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
      })
    })

    observe({
      if (input$clear == 0)return()
      isolate({
        newText = ""
        updateTextInput(session,"textIn",value=newText)
        v$df = text_to_dataframe(newText,badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
      })
    })

    observe({
      if (input$word1 == 0)return()
      isolate({
        newText = paste(input$textIn,v$df[[1]][1])
        updateTextInput(session,"textIn",value=newText)
        v$df = text_to_dataframe(newText,badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
      })
    })

    observe({
      if (input$word2 == 0)return()
      isolate({
        newText = paste(input$textIn,v$df[[1]][2])
        updateTextInput(session,"textIn",value=newText)
        v$df = text_to_dataframe(newText,badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
      })
    })
    
    observe({
      if (input$word3 == 0)return()
      isolate({
        newText = paste(input$textIn,v$df[[1]][3])
        updateTextInput(session,"textIn",value=newText)
        v$df = text_to_dataframe(newText,badwords,vocab,discountedUnigram,discountedBigram,collapsedTrigram)
      })
    })
    
    output$predictions = renderTable(v$df)
    output$suggest1 = renderText(v$df[[1]][1])
    output$suggest2 = renderText(v$df[[1]][2])
    output$suggest3 = renderText(v$df[[1]][3])
})