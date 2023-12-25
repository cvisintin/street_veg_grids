# # library(shiny)
# # 
# # ui <- basicPage(
# #   plotOutput("plot1",
# #              click = "plot_click",
# #              dblclick = "plot_dblclick",
# #              hover = "plot_hover",
# #              brush = "plot_brush"
# #   ),
# #   verbatimTextOutput("info")
# # )
# # 
# # server <- function(input, output) {
# #   library(jpeg)
# #   prev_vals <- NULL
# #   structures <- reactiveValues(data = data.frame(box_id = numeric(), xmin = numeric(), ymin = numeric(), xmax = numeric(), xmax = numeric()))
# #   
# #   output$plot1 <- renderPlot({
# #     img <- readJPEG("park_street_full.jpg", native = TRUE)
# #     dim_img <- dim(img)
# #     plot(1:640, type='n')
# #     rasterImage(img,1,1,640,640)
# #     if (nrow(structures$data) > 0) {
# #       r <- structures$data
# #       rect(r$xmin, r$ymin, r$xmax, r$ymax, border = "red")
# #     }
# #   }, height = dim_img[1], width = dim_img[2])
# #   
# #   observe({
# #     e <- input$plot_brush
# #     if (!is.null(e)) {
# #       vals <- data.frame(xmin = round(e$xmin, 1), ymin = round(e$ymin, 1), xmax = round(e$xmax, 1), ymax = round(e$ymax, 1))
# #       if (identical(vals,prev_vals)) return() #We dont want to change anything if the values havent changed.
# #       structures$data <- rbind(structures$data,cbind(data.frame(box_id = nrow(structures$data)+1),vals))
# #       prev_vals <<- vals
# #     }
# #   })
# #   
# #   output$info <- renderText({
# #     
# #     xy_str <- function(e) {
# #       if(is.null(e)) return("NULL\n")
# #       paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
# #     }
# #     
# #     
# #     xy_range_str <- function(e) {
# #       if(is.null(e)) return("NULL\n")
# #       paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
# #              " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
# #     }
# #     
# #     paste0(
# #       "click: ", xy_str(input$plot_click),
# #       "dblclick: ", xy_str(input$plot_dblclick),
# #       "hover: ", xy_str(input$plot_hover),
# #       "brush: ", xy_range_str(input$plot_brush)
# #     )
# #     
# #   })
# # }
# # 
# # shinyApp(ui, server)
# 
# #########################################
# 
# library(shiny)
# library(tidyverse)
# 
# ui <- basicPage(
#   plotOutput("plot1", click = "plot_click"),
#   tableOutput("table"),
#   textInput("polygon_name", label = "Polygon name", value = "polygon 1")
# )
# 
# server <- function(input, output) {
#   coords <- reactiveVal(value = tibble(x = numeric(), y = numeric(), name = character()))
#   
#   observeEvent(input$plot_click, {
#     add_row(coords(),
#             x = isolate(input$plot_click$x),
#             y = isolate(input$plot_click$y),
#             name = isolate(input$polygon_name)
#     ) %>% coords()
#   })
#   
#   output$plot1 <- renderPlot({
#     plot(r)
#     
#     coords() %>%
#       nest(-name) %>%
#       deframe() %>%
#       map(~ polygon(.x$x, .x$y))
#   })
#   
#   output$table <- renderTable(coords())
# }
# 
# shinyApp(ui, server)
# 
# #########################################
# 
library(shiny)
library(base64enc)

options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  fileInput("upload", "Upload image", accept = "image/png"),
  uiOutput("image")
)

server <- function(input, output){

  base64 <- reactive({
    inFile <- input[["upload"]]
    if(!is.null(inFile)){
      dataURI(file = inFile$datapath, mime = "image/png")
    }
  })

  output[["image"]] <- renderUI({
    if(!is.null(base64())){
      tags$div(
        tags$img(src= base64(), width="100%"),
        style = "width: 800px;"
      )
    }
  })




}

shinyApp(ui, server)
# 
# ###################################
# 
# library(shiny)
# library(tidyverse)
# library(raster)
# library(ggplot2)
# library(cowplot)
# library(magick)
# #library(shinyjs)
# #library(base64enc)
# 
# options(shiny.maxRequestSize = 30*1024^2,
#         shiny.launch.browser = .rs.invokeShinyWindowExternal)
# 
# # jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
# #         var element = document.documentElement,
# #           enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
# #           exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
# #         if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
# #           enterFS.call(element);
# #         } else {
# #           exitFS.call(document);
# #         }
# #       }'
# 
# ui <- fluidPage(
#   # useShinyjs(),
#   # extendShinyjs(text = jsToggleFS),
#   fileInput("upload", "Upload image", accept = "image/png"),
#   #uiOutput("image"),
#   #imageOutput("myImage"),
#   imageOutput("plot", click = "plot_click"),
#   tableOutput("table")
# )
# 
# server <- function(input, output){
#   library(ggplot2)
#   library(cowplot)
#   
#   coords <- reactiveVal(value = tibble(x = numeric(), y = numeric(), name = character()))
#   
#   observeEvent(input$plot_click, {
#     add_row(coords(),
#             x = isolate(input$plot_click$x),
#             y = isolate(input$plot_click$y),
#             name = 'A'
#     ) %>% coords()
#   })
#   
#   # base64 <- reactive({
#   #   inFile <- input[["upload"]]
#   #   if(!is.null(inFile)){
#   #     dataURI(file = inFile$datapath, mime = "image/png")
#   #   }
#   # })
#   # 
#   # output[["image"]] <- renderUI({
#   #   if(!is.null(base64())){
#   #     tags$div(
#   #       tags$img(src= base64(), width="100%"),
#   #       style = "width: 800px;"
#   #     )
#   #   }
#   # })
#   
#   # output$myImage <- renderImage({
#   #   # A temp file to save the output.
#   #   # This file will be removed later by renderImage
#   #   outfile <- input[["upload"]]
#   #   
#   #   # Generate the PNG
#   #   png(outfile$datapath, width = 400, height = 300)
#   #   hist(rnorm(input$obs), main = "Generated in renderImage()")
#   #   dev.off()
#   #   
#   #   # Return a list containing the filename
#   #   list(src = outfile,
#   #        contentType = 'image/png',
#   #        width = 400,
#   #        height = 300,
#   #        alt = "This is alternate text")
#   # }, deleteFile = TRUE)
#   
#   # output$plot <- renderImage({
#   #   filename <- as.character(input[["upload"]]$datapath)
#   #   
#   #   # Return a list containing the filename
#   #   list(src = filename, alt = "Alternate text")
#   # }, deleteFile = FALSE)
#   
#   output$plot <- renderPlot({
#     filename <- as.character(input[["upload"]]$datapath)
#     # library(png)
#     # library(imager)
#     
#     img <- readPNG(filename)    
#     # img <- load.image(filename)
#     
#     img_dim <- dim(img)
#     lim <- par()
#     
#     # plot.new()
#     # par(mar = rep(0, 4))
#     # plot.window(xlim = c(0, lim$usr[3]), ylim = c(lim$usr[2], lim$usr[4]))
#     # rasterImage(as.raster(img), lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
#     # plot(as.raster(img))
#     
#     # linecolor <- "white"
#     # 
#     # p <- ggplot(coords, aes(x, y, label = row.names(coords))) + 
#     #   #annotation_custom(bg_img) +
#     #   geom_point(colour = "black", size = 0.5) +
#     #   # geom_text(colour = "black", nudge_x = -5, nudge_y = -5, size = 2.0) +
#     #   geom_line(data = coords[c("L", "F"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("F", "B"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("B", "K"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("K", "L"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   geom_line(data = coords[c("Z", "V"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("Y", "U"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("X", "T"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("W", "S"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   geom_line(data = coords[c("AE", "AA"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AF", "AB"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AG", "AC"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AH", "AD"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   geom_line(data = coords[c("J", "E"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("E", "D"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("D", "I"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("I", "J"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   geom_line(data = coords[c("AT", "AP"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AS", "AO"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AR", "AN"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AQ", "AM"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   geom_line(data = coords[c("AY", "AU"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("AZ", "AV"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("BA", "AW"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   geom_line(data = coords[c("BB", "AX"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
#     #   
#     #   scale_x_continuous(expand = c(0, 0), lim = c(0, dim(img)[2])) +
#     #   scale_y_continuous(expand = c(0, 0), lim = c(0, dim(img)[1])) +
#     #   theme_void() +
#     #   theme(aspect.ratio = nrow(img) / ncol(img))
#     
#     p <- ggplot() +
#       scale_x_continuous(expand = c(0, 0), lim = c(0, dim(img)[2])) +
#       scale_y_continuous(expand = c(0, 0), lim = c(0, dim(img)[1])) +
#       theme_void() +
#       theme(aspect.ratio = nrow(img) / ncol(img))
#     
#     ggdraw() + draw_image(filename)# + draw_plot(p)
#     
#     coords() %>%
#       nest(-name) %>%
#       deframe() %>%
#       map(~ polygon(.x$x, .x$y))
#   })
#   
#   output$table <- renderTable(coords())
# }
# 
# shinyApp(ui, server)
