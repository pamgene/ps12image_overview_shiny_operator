library(shiny)
library(shinyjs)

ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
  tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
  uiOutput("reacOut"),
  title = "Image Overview"
))