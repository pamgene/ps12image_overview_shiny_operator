library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
  options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
  options("tercen.username"= "admin")
  options("tercen.password"= "admin")
  ctx <- tercenCtx()
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
    uiOutput("reacOut"),
    title = "Image Overview"))

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getData(session)
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Image overview</center></h3>"),
      fluidRow(
        column(2, selectizeInput("colId", "Col", choices = c())),
        column(2, selectizeInput("exposureTimeId", "Filter_Exposure Time", choices = c())),
        column(2, selectizeInput("cycleId", "Cycle", choices = c())),
        column(2, selectizeInput("quantitationTypeId", "Quantitation Type", choices = c())),
        column(2, selectizeInput("viewTypeId", "View Type", choices = c()))
      ),
      fluidRow(
        column(12, plotOutput("plot")))
    )
  })
  
})

getData <- function(session){
  ctx           <- getCtx(session)
  # TODO
  
  return(NULL)
}

runApp(shinyApp(ui, server))  
