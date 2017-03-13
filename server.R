# This is the server logic for a Shiny web application.


library(shiny)
source('predict_next.r')

shinyServer(function(input, output) {
    dataOutput <- reactive({
            predict_next(input$entry,maxResults = input$maxResults)
        })
    
    output$predict <- renderTable({
        dataOutput()
    },rownames = TRUE,colnames = FALSE,bordered = TRUE,width = 200)
    
    output$entry <- renderText({
        input$entry
    })
   
})