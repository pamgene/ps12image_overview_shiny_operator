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
        column(2),
        column(2, selectizeInput("cycleId", "Cycle", choices = c(LOADING_DATA))),
        column(2, selectizeInput("exposureTimeId", "Filter_Exposure Time", choices = c(LOADING_DATA))),
        column(2)
      ),
      tags$p(),
      fluidRow(
        column(1),
        column(10, plotOutput("plot")))
    )
  })
  
  get_plot_width <- reactive({
    df    <- dataInput()
    ncols <- length(unique(df$Barcode))
    ncols * 270
  })
  
  get_plot_height <- reactive({
    df    <- dataInput()
    nrows <- length(unique(df$Row))
    nrows * 200
  })
  
  output$plot <- renderPlot({
    df <- dataInput()
    
    # filtering
    if (input$exposureTimeId != LOADING_DATA && input$cycleId != LOADING_DATA) {
      df <- df %>%
        filter(`Exposure Time` == input$exposureTimeId) %>%
        filter(Cycle == input$cycleId)
      
      # convert to png and display in grid
      if (nrow(df) > 0) {
        tiff_images <- df$Image
        png_img_dir <- "png"
        if (dir.exists(png_img_dir)) {
          system(paste("rm -rf", png_img_dir))
        }
        dir.create(png_img_dir)
        lapply(seq_along(tiff_images), FUN = function(y, i) {
          tiff_file <- y[i]
          png_file  <- file.path(png_img_dir, paste0("out", i, ".png"))
          # note we need to shift the input 4 bits to the left -> multiply by 2^4 = 16
          png::writePNG(suppressWarnings(tiff::readTIFF(tiff_file) * 16), png_file)
        }, y = tiff_images)
        
        png_files <- list.files(path = png_img_dir, pattern = "*.png", full.names = TRUE)
        png_files <- normalizePath(png_files)
        png_files <- png_files[file.exists(png_files)]
        pngs      <- lapply(png_files, readPNG)
        asGrobs   <- lapply(pngs, FUN = function(png) { rasterGrob(png, height = 0.99)  })
        nrows     <- length(unique(df$Row))
        c_titles  <- unique(df$Barcode)
        r_titles  <- as.character(seq(nrows))
        theme     <- ttheme_minimal(base_size = 16)
        combinedPlot <- rbind(tableGrob(t(c_titles), theme = theme, rows = ""), 
                              cbind(tableGrob(r_titles, theme = theme),
                                    arrangeGrob(grobs = asGrobs, nrow = nrows), size = "last"), size = "last")
        grid.newpage()
        grid.draw(combinedPlot)
      }
    }
  }, height = get_plot_height, width = get_plot_width)
  
  observeEvent(values$df, {
    df <- values$df
    updateSelectizeInput(session, "cycleId", choices = sort(unique(df$Cycle)))
    updateSelectizeInput(session, "exposureTimeId", choices = sort(unique(df$`Exposure Time`)))
  })
})

TAG_LIST  <- list("date_time" = "DateTime", "barcode" = "Barcode", "col" = "Col", "cycle" = "Cycle", "exposure time" = "Exposure Time", "filter" = "Filter", 
                  "ps12" = "PS12", "row" = "Row", "temperature" = "Temperature", "timestamp" = "Timestamp", "instrument unit" = "Instrument Unit", "run id" = "Run ID")
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
