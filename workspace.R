library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(tiff)
library(ijtiff)
library(gridExtra)
library(png)
library(grid)

############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  options("tercen.workflowId"= "050e773677ecc404aa5d5a7580016b7d")
  options("tercen.stepId"= "c8d5a345-d600-4b07-88cc-aae49d242ae7")
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
  
  values <- reactiveValues()
  
  dataInput <- reactive({
    values$df <- getData(session)
    values$df
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Image overview</center></h3>"),
      fluidRow(
        column(1),
        column(2, selectizeInput("colId", "Col", choices = c(LOADING_DATA))),
        column(2, selectizeInput("exposureTimeId", "Filter_Exposure Time", choices = c(LOADING_DATA))),
        column(2, selectizeInput("cycleId", "Cycle", choices = c(LOADING_DATA))),
        column(2, selectizeInput("quantitationTypeId", "Quantitation Type", choices = c(LOADING_DATA))),
        column(2, selectizeInput("viewTypeId", "View Type", choices = c(LOADING_DATA))),
        column(1)
      ),
      tags$p(),
      fluidRow(
        column(12, plotOutput("plot")))
    )
  })
  
  output$plot <- renderPlot({
    df <- dataInput()
    
    # TODO filtering
    
    # convert to png and display in grid
    tiff_images <- df$Image[1:3]
    lapply(seq_along(tiff_images), FUN = function(y, i) {
      tiff_file <- y[i]
      png_file  <- paste0("png/out", i, ".png")
      png::writePNG(suppressWarnings(tiff::readTIFF(tiff_file)), png_file)  
    }, y = tiff_images)
    
    png_files <- list.files(path = "png", pattern = "*.png", full.names = TRUE)
    png_files <- normalizePath(png_files)
    png_files <- png_files[file.exists(png_files)]
    pngs      <- lapply(png_files, readPNG)
    asGrobs   <- lapply(pngs, rasterGrob)
    nrows     <- length(tiff_images) %/% 3
    grid.arrange(grobs=asGrobs, nrow = nrows)
  })
  
  observeEvent(values$df, {
    df <- values$df
    updateSelectizeInput(session, "colId", choices = sort(unique(df$Col)))
    updateSelectizeInput(session, "exposureTimeId", choices = sort(unique(df$`Exposure Time`)))
    updateSelectizeInput(session, "cycleId", choices = sort(unique(df$Cycle)))
    updateSelectizeInput(session, "quantitationTypeId", choices = c("Image"))
    updateSelectizeInput(session, "viewTypeId", choices = c("Barcode - Row"))
  })
})

TAG_LIST  <- list("date_time" = "DateTime", "barcode" = "Barcode", "col" = "Col", "cycle" = "Cycle", "exposure time" = "Exposure Time", "filter" = "Filter", 
                  "ps12" = "PS12", "row" = "Row", "temperature" = "Temperature", "timestamp" = "Timestamp", "instrument unit" = "Instrument Unit", "protocol id" = "Protocol ID")
TAG_NAMES <- as.vector(unlist(TAG_LIST))
IMAGE_COL <- "Image"
LOADING_DATA <- "Loading data"

get_file_tags <- function(filename) {
  tags <- NULL
  all_tags <- suppressWarnings(ijtiff::read_tags(filename))
  if (!is.null(all_tags) && !is.null(names(all_tags)) && "frame1" %in% names(all_tags)) {
    tags <- all_tags$frame1
    tags <- tags[names(TAG_LIST)]
    names(tags) <- TAG_NAMES
  }
  tags
}

doc_to_data <- function(df, ctx){
  #1. extract files
  docId = df$documentId[1]
  doc = ctx$client$fileService$get(docId)
  filename = tempfile()
  writeBin(ctx$client$fileService$download(docId), filename)
  on.exit(unlink(filename))
  
  # unzip if archive
  if(length(grep(".zip", doc$name)) > 0) {
    tmpdir <- tempfile()
    unzip(filename, exdir = tmpdir)
    f.names <- list.files(file.path(list.files(tmpdir, full.names = TRUE), "ImageResults"), full.names = TRUE)
  } else {
    f.names <- filename
  }
  
  # read tags
  result <- do.call(rbind, lapply(f.names, FUN = function(filename) {
    tags <- get_file_tags(filename)
    image        <- filename
    names(image) <- IMAGE_COL
    as.data.frame(t(as.data.frame(c(image, unlist(tags)))))
  }))
  
  result %>% 
    mutate(path = "ImageResults") %>% 
    mutate(documentId = docId) %>%
    mutate_at(vars(Col, Cycle, 'Exposure Time', Row, Temperature), .funs = function(x) { as.numeric(as.character(x)) }) %>%
    select(documentId, path, all_of(IMAGE_COL), all_of(TAG_NAMES))
}

getData <- function(session){
  ctx = getCtx(session)
  if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 
  
  result <- ctx$cselect() %>%
    doc_to_data(ctx)

  return(result)
}

runApp(shinyApp(ui, server))  
