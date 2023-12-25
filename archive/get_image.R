library(shiny)
library(shinyFiles)
library(jpeg)
library(raster)
library(tcltk)

# ui <- fluidPage(
#   shinyDirButton("Btn_GetFolder", "Choose a folder" ,
#                  title = "Please select a folder:", multiple = FALSE,
#                  buttonType = "default", class = NULL),
#   
#   textOutput("txt_file")     
# )
# 
# 
# server <- function(input,output,session){
#   
#   volumes = c(home = "/home/casey/Research/Projects/Arch_Ecology/Everyday_Nature/")
#   observe({  
#     shinyDirChoose(input, "Btn_GetFolder",
#                    roots = volumes)
#   })
#   
#   output$txt_file <- renderText({
#     file_selected <- parseDirPath(roots = volumes, input$Btn_GetFolder)
#   })
# }
# 
# shinyApp(ui = ui, server = server)

##################################################

# ui <- fluidPage(
#   titlePanel("Input"),
#   mainPanel(
#     shinyFilesButton("file", "Choose File", "Choose a file", multiple = FALSE),
#     verbatimTextOutput("file"),
#     plotOutput("plot", click = "plot_click")
#   )
# )
# 
# server <- function(input, output, session) {  
#   roots <- getVolumes()
#   shinyFileChoose(input, "file", roots = roots)
#   file <- reactive({
#     req(input$file)
#     parseFilePaths(roots, input$file)
#   })
#   output$file <- renderPrint(file()$datapath)
#   output$plot <- renderPlot({
#     req(input$file)
#     img <- ifelse(is.null(file()$datapath), NULL, readJPEG(file()$datapath))
#     plot(as.raster(img))})
# }
# 
# shinyApp(ui = ui, server = server)

##################################################

ui <- fluidPage(
  fluidRow(column(width = 12,
                  h4("Upload image"),
                  fileInput("upload", NULL, accept = "image/jpg"),
                  plotOutput("plot", click = "plot_click"))
  )
)

server <- function(input, output, session) {
  # values <- reactiveValues()
  # values$DTimg <- data.frame(w = numeric(),
  #                         h = numeric())
  img_file <- reactive({
    req(input$upload)
    path <- input$upload$datapath
    img <- readJPEG(path)
    # values$DTimg$w <- dim(img)[2]
    # values$DTimg$h <- dim(img)[1]
    # assign("img_width", dim(img)[2], envir = globalenv())
    # assign("img_height", dim(img)[1], envir = globalenv())
    img
  })
  
  img_width <- reactive({
    req(img_file())
    img <- img_file()
    as.numeric(dim(img)[2])
  })

  img_height <- reactive({
    req(img_file())
    img <- img_file()
    as.numeric(dim(img)[1])
  })
  
  # observeEvent(values$DTimg$w, {
  #   assign("img_width", values$DTimg$w, envir = globalenv())
  # })
  # 
  # observeEvent(values$DTimg$h, {
  #   assign("img_height", values$DTimg$h, envir = globalenv())
  # })
    
  w <- 1600
  
  observe({
    output$plot <- renderPlot({
    req(img_file())
    img <- img_file()
    plot(as.raster(img))}, res = 100, width = w, height = round((w / img_width()) * img_height())
    )
  })
  
}

shinyApp(ui = ui, server = server)

####################################################

# ui <- fluidPage(
#   verbatimTextOutput("printFilePath"),
#   actionButton("inputFileButton", "Select File"),
#   plotOutput("plot", click = "plot_click")
# )
# 
# server <- function(input, output) {      
#   observeEvent(input$inputFileButton, {
#     req(input$inputFileButton)
#     selectedFilePath <- tk_choose.files(caption = "Select the Example Input File")
#     output$printFilePath <- renderPrint(selectedFilePath)
#     assign("selectedFilePath", selectedFilePath, .GlobalEnv)
#   })
#   
#   img <- readJPEG(selectedFilePath)
#   
#   output$plot <- renderPlot({
#     req(img)
#     plot(as.raster(img))})
# }
# 
# shinyApp(ui, server)