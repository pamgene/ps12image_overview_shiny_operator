library(shiny)
library(tercen)
library(dplyr)
library(tidyr)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getData(session)
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Image overview</center></h3>"),
      fluidRow(
        column(1),
        column(2, selectizeInput("colId", "Col", choices = c())),
        column(2, selectizeInput("exposureTimeId", "Filter_Exposure Time", choices = c())),
        column(2, selectizeInput("cycleId", "Cycle", choices = c())),
        column(2, selectizeInput("quantitationTypeId", "Quantitation Type", choices = c())),
        column(2, selectizeInput("viewTypeId", "View Type", choices = c())),
        column(1)
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
