
library(shiny)
source("./project_predicting.R")


####################### UI Section ####################################
ui <- fluidPage(
  tags$head(
    tags$style("label{font-family: BentonSans Book;}")
  ),
  img(src = "https://czech-invest.eu/wp-content/uploads/2019/03/bill-anastas-241386-unsplash.jpg",height="70%", width="70%"),
  titlePanel("Hotel Reviews' Rating Prediction "),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text",label="Please enter a review to predict its rating"),
      actionButton("goButton", "Predict"),
      br(),br(),
      textInput("Rating","Predicted Rating:"),
      actionButton("reset_input", "Reset"),
      tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
             left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #CCFF68;
             z-index: 105;
           }"
  )),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage"))),
    mainPanel(
      verbatimTextOutput("ntext")
    
  ))
######################## End of UI Section ##########################

####################### Server Section ###############################

server <- shinyServer(function(input, output,session) {
  
  ntext <- eventReactive(input$goButton, {input$text})
  
  observeEvent(input$goButton,{
    test <- preprocessing_test(ntext())
    model<-readRDS("fitmodel.rds")
    pred.rf<-predict(object=model, newdata=test)
    updateTextInput(session, "Rating", value = as.character(pred.rf))
  })
  observeEvent(input$reset_input,{
    updateTextInput(session, "text",value="")
    updateTextInput(session, "Rating",value="")
  })
})

################### End of Server Section #############################

shinyApp(ui = ui, server = server)
