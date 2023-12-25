library(shiny)
library(jpeg)
library(png)

options(shiny.maxRequestSize = 30*1024^2,
        shiny.launch.browser = .rs.invokeShinyWindowExternal)

ui <- fluidPage(
  fluidRow(column(width = 10,
                  h4("Upload image (PNG or JPG)"),
                  fileInput("upload", NULL, accept = "image/*"),
                  h4("Click plot to add three points"),
                  actionButton("rem_points", "Clear all points"),
                  actionButton("build_grids", "Build perspective grids"),
                  plotOutput("plot", click = "plot_click")),
           column(width = 2,
                  h4("Table of points on plot"),
                  div(
                    tableOutput("table"),
                    tags$head(tags$style(type="text/css", "#table table td {line-height:80%}")),
                    style = "font-size:60%"
                  )
           )
  )
)
# div(tableOutput("table")), style = "font-size:60%; padding-top:0px; padding-bottom:0px;"))

server = function(input, output){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric())
  
  ## 2. load up image and set dimension parameters##
  img_file <- reactive({
    req(input$upload)
    path <- input$upload$datapath
    if(grepl("jpg", path))img <- readJPEG(path)
    if(grepl("png", path))img <- readPNG(path)
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
  
  ## 3. Create a plot ##
  # img <- readJPEG("park_street_full.jpg")
  
  w <- 1600
  
  observe({
    output$plot <- renderPlot({
      
      req(img_file())
      img <- img_file()
      
      plot(as.raster(img))
      
      lines(values$DT[c("1", "2"), ], lty = "B5", lwd = 2)
      lines(values$DT[c("1", "3"), ], lty = "B5", lwd = 2)
      
      linecolor <- "white"
      
      lines(values$DT[c("L", "F"), ], col = linecolor)
      lines(values$DT[c("F", "B"), ], col = linecolor)
      lines(values$DT[c("B", "K"), ], col = linecolor)
      lines(values$DT[c("K", "L"), ], col = linecolor)
      
      lines(values$DT[c("Z", "V"), ], col = linecolor)
      lines(values$DT[c("Y", "U"), ], col = linecolor)
      lines(values$DT[c("X", "T"), ], col = linecolor)
      lines(values$DT[c("W", "S"), ], col = linecolor)
      
      lines(values$DT[c("AE", "AA"), ], col = linecolor)
      lines(values$DT[c("AF", "AB"), ], col = linecolor)
      lines(values$DT[c("AG", "AC"), ], col = linecolor)
      lines(values$DT[c("AH", "AD"), ], col = linecolor)
      
      lines(values$DT[c("J", "E"), ], col = linecolor)
      lines(values$DT[c("E", "D"), ], col = linecolor)
      lines(values$DT[c("D", "I"), ], col = linecolor)
      lines(values$DT[c("I", "J"), ], col = linecolor)
      
      lines(values$DT[c("AT", "AP"), ], col = linecolor)
      lines(values$DT[c("AS", "AO"), ], col = linecolor)
      lines(values$DT[c("AR", "AN"), ], col = linecolor)
      lines(values$DT[c("AQ", "AM"), ], col = linecolor)
      
      lines(values$DT[c("AY", "AU"), ], col = linecolor)
      lines(values$DT[c("AZ", "AV"), ], col = linecolor)
      lines(values$DT[c("BA", "AW"), ], col = linecolor)
      lines(values$DT[c("BB", "AX"), ], col = linecolor)
    }, res = 100, width = w, height = round((w / img_width()) * img_height()))
    
  })
  
  ## 4. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = input$plot_click$x,
                          y = input$plot_click$y)
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
  })
  
  ## 5. remove row on actionButton click ##
  observeEvent(input$rem_points, {
    rem_row <- values$DT[-c(1:nrow(values$DT)), ]
    values$DT <- rem_row
  })
  
  ## 6. render a table of the growing dataframe ##
  output$table <- renderTable({
    cbind(values$DT, "name" = row.names(values$DT))
  }, spacing = "xs")
  
  ## 7. build grids 
  observeEvent(input$build_grids, {
    
    if(nrow(values$DT) == 3) {
      # Define custom function for finding intersections of lines
      intersect_coord <- function(l1, l2){
        x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
        y <- l1[1] + l1[2] * x
        return(xy = c(x, y))
      }
      
      # Initiate data.frame for coordinates
      coords <- data.frame(x = rep(NA, 54), y = rep(NA, 54))
      row.names(coords) <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS[1:2]))
      
      # Record user selected coordinates
      coords$x[1:3] <- values$DT$x
      coords$y[1:3] <- values$DT$y
      
      # Determine angle between normal and coordinate C from coordinate A
      theta1 <- atan((coords["A", "x"] - coords["C", "x"]) / coords["A", "y"])
      
      # Determine coordinate D
      coords["D", ] <- c(coords["A", "x"] - tan(theta1) * (coords["A", "y"] - coords["B", "y"]), coords["B", "y"])
      
      # Draw vertical line from coordinate D to top of image to get coordinate E
      coords["E", ] <- c(coords["D", "x"], img_height())
      
      # Draw horizontal line from coordinate E to align with coordinate B to get coordinate F
      coords["F", ] <- c(coords["B", "x"], coords["E", "y"]) 
      
      # Draw vertical line down from coordinate A to intersect line B-D to get coordinate G
      coords["G", ] <- c(coords["A", "x"], coords["B", "y"])
      
      # Determine coordinate at X proportional distance along line G-A to get coordinate H
      X <- 0.67
      coords["H", ] <- c(coords["A", "x"], (coords["A", "y"] - coords["G", "y"]) * X +  coords["G", "y"])
      
      # Determine coordinate I
      coords["I", ] <- c(coords["A", "x"] -  tan(theta1) * (coords["A", "y"] - coords["H", "y"]), coords["H", "y"])
      
      # Determine angle between horizon and coordinate E from coordinate A
      theta2 <- atan((coords["E", "y"] - coords["A", "y"]) / (coords["A", "x"] - coords["D", "x"]))
      
      # Determine coordinate J
      coords["J", ] <- c(coords["I", "x"], coords["A", "y"] + tan(theta2) * (coords["A", "x"] - coords["I", "x"]))
      
      # Determine angle between normal and coordinate B from coordinate A
      theta3 <- atan((coords["B", "x"] - coords["A", "x"]) / (coords["A", "y"] - coords["B", "y"]))
      
      # Determine coordinate K
      coords["K", ] <- c(coords["A", "x"] + tan(theta3) * (coords["A", "y"] - coords["H", "y"]), coords["I", "y"])
      
      # Determine angle between horizon and coordinate F from coordinate A
      theta4 <- atan((coords["F", "y"] - coords["A", "y"]) / (coords["F", "x"] - coords["A", "x"]))
      
      # Determine coordinate L
      coords["L", ] <- c(coords["K", "x"], coords["A", "y"] + tan(theta4) * (coords["K", "x"] - coords["A", "x"]))
      
      # Determine angle between horizontal and coordinate K from coordinate A
      theta5 <- atan((coords["K", "y"] - coords["G", "y"]) / (coords["K", "x"] - coords["G", "x"]))
      
      # Determine coordinate M
      coords["M", ] <- c((coords["A", "x"] + ((coords["A", "y"] - coords["G", "y"]) / tan(theta5))), coords["A", "y"])
      
      # Determine angle between horizontal and coordinate K from coordinate A
      theta6 <- atan((coords["I", "y"] - coords["G", "y"]) / (coords["G", "x"] - coords["I", "x"]))
      
      # Determine coordinate N
      coords["N", ] <- c((coords["A", "x"] - ((coords["A", "y"] - coords["G", "y"]) / tan(theta6))), coords["A", "y"])
      
      #### Right-hand grid ####
      
      # Determine coordinates O, P, Q and R
      GB_seg_len <- (coords["B", "x"] - coords["G", "x"]) / 5
      len_additions <- GB_seg_len * 1:4
      coord_names <- c("O", "P", "Q", "R")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["G", "x"] + len_additions[which(coord_names == i)], coords["G", "y"])  
      }
      
      # Determine coordinates S, T, U and V
      lineAB_eq <- lm(coords[c("A", "B"), "y"] ~ coords[c("A", "B"), "x"])
      coord_names <- c("S", "T", "U", "V")
      ref_coords <- c("O", "P", "Q", "R")
      for (i in coord_names) {
        line_eq <- lm(coords[c(ref_coords[which(coord_names == i)], "M"), "y"] ~ coords[c(ref_coords[which(coord_names == i)], "M"), "x"])
        coords[i, ] <- intersect_coord(lineAB_eq$coefficients, line_eq$coefficients)
      }
      
      # Determine coordinates W, X, Y and Z
      APF <- approxfun(coords[c("A", "F"), "x"], coords[c("A", "F"), "y"])
      coord_names <- c("W", "X", "Y", "Z")
      ref_coords <- c("S", "T", "U", "V")
      for (i in coord_names) {
        coords[i, ] <- c(coords[ref_coords[which(coord_names == i)], "x"], APF(coords[ref_coords[which(coord_names == i)], "x"]))
      }
      
      # Determine coordinates AA, AB, AC and AD
      FB_seg_len <- (coords["F", "y"] - coords["B", "y"]) / 5
      len_additions <- FB_seg_len * 1:4
      coord_names <- c("AA", "AB", "AC", "AD")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["B", "x"], coords["B", "y"] + len_additions[which(coord_names == i)])  
      }
      
      # Determine coordinates AE, AF, AG and AH
      LK_seg_len <- (coords["L", "y"] - coords["K", "y"]) / 5
      len_additions <- LK_seg_len * 1:4
      coord_names <- c("AE", "AF", "AG", "AH")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["K", "x"], coords["K", "y"] + len_additions[which(coord_names == i)])  
      }
      
      #### Left-hand grid ####
      
      # Determine coordinates AI, AJ, AK and AL
      GD_seg_len <- (coords["G", "x"] - coords["D", "x"]) / 5
      len_additions <- GD_seg_len * 1:4
      coord_names <- c("AI", "AJ", "AK", "AL")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["G", "x"] - len_additions[which(coord_names == i)], coords["G", "y"])  
      }
      
      # Determine coordinates AM, AN, AO and AP
      lineAD_eq <- lm(coords[c("A", "D"), "y"] ~ coords[c("A", "D"), "x"])
      coord_names <- c("AM", "AN", "AO", "AP")
      ref_coords <- c("AI", "AJ", "AK", "AL")
      for (i in coord_names) {
        line_eq <- lm(coords[c(ref_coords[which(coord_names == i)], "N"), "y"] ~ coords[c(ref_coords[which(coord_names == i)], "N"), "x"])
        coords[i, ] <- intersect_coord(lineAD_eq$coefficients, line_eq$coefficients)
      }
      
      # Determine coordinates AQ, AR, AS and AT
      APF <- approxfun(coords[c("A", "E"), "x"], coords[c("A", "E"), "y"])
      coord_names <- c("AQ", "AR", "AS", "AT")
      ref_coords <- c("AM", "AN", "AO", "AP")
      for (i in coord_names) {
        coords[i, ] <- c(coords[ref_coords[which(coord_names == i)], "x"], APF(coords[ref_coords[which(coord_names == i)], "x"]))
      }
      
      # Determine coordinates AU, AV, AW and AX
      ED_seg_len <- (coords["F", "y"] - coords["B", "y"]) / 5
      len_additions <- ED_seg_len * 1:4
      coord_names <- c("AU", "AV", "AW", "AX")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["D", "x"], coords["D", "y"] + len_additions[which(coord_names == i)])  
      }
      
      # Determine coordinates AY, AZ, BA and BB
      JI_seg_len <- (coords["J", "y"] - coords["I", "y"]) / 5
      len_additions <- JI_seg_len * 1:4
      coord_names <- c("AY", "AZ", "BA", "BB")
      
      for (i in coord_names) {
        coords[i, ] <- c(coords["I", "x"], coords["I", "y"] + len_additions[which(coord_names == i)])  
      }
      
      values$DT <- coords
    }
  })
  
}

shinyApp(ui, server)