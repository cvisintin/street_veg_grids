library(png)
library(grid)
library(ggplot2)
library(cowplot)

# Define custom function for finding intersections of lines
intersect_coord <- function(l1, l2){
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(xy = c(x, y))
}

# Load image
img <- png::readPNG('park_street_full.png')
# bg_img <- grid::rasterGrob(img, width = c(0, dim(img)[2]), height = c(0, dim(img)[1]))

# Determine image boundary coordinates
img_coords <- data.frame(x = c(0, dim(img)[2], 0, dim(img)[2]), y = c(0, 0, dim(img)[1], dim(img)[1]))
row.names(img_coords) <- c("LL", "LR", "UL", "UR")

# Initiate data.frame for coordinates
sel_coords <- data.frame(x = rep(NA, 54), y = rep(NA, 54))
row.names(sel_coords) <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS[1:2]))

# User selected coordinates
sel_coords$x[1:3] <- c(1150, 1920, 335)
sel_coords$y[1:3] <- c(425, 225, 0)

# Combine all coordinates
coords <- rbind(img_coords, sel_coords)

# Determine angle between normal and coordinate C from coordinate A
theta1 <- atan((coords["A", "x"] - coords["C", "x"]) / coords["A", "y"])

# Determine coordinate D
coords["D", ] <- c(coords["A", "x"] - tan(theta1) * (coords["A", "y"] - coords["B", "y"]), coords["B", "y"])

# Draw vertical line from coordinate D to top of image to get coordinate E
coords["E", ] <- c(coords["D", "x"], coords["UL", "y"])

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

#######
linecolor <- "white"

p <- ggplot(coords, aes(x, y, label = row.names(coords))) + 
  #annotation_custom(bg_img) +
  geom_point(colour = "black", size = 0.5) +
  # geom_text(colour = "black", nudge_x = -5, nudge_y = -5, size = 2.0) +
  geom_line(data = coords[c("L", "F"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("F", "B"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("B", "K"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("K", "L"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  geom_line(data = coords[c("Z", "V"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("Y", "U"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("X", "T"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("W", "S"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  geom_line(data = coords[c("AE", "AA"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AF", "AB"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AG", "AC"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AH", "AD"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  geom_line(data = coords[c("J", "E"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("E", "D"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("D", "I"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("I", "J"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  geom_line(data = coords[c("AT", "AP"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AS", "AO"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AR", "AN"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AQ", "AM"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  geom_line(data = coords[c("AY", "AU"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("AZ", "AV"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("BA", "AW"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  geom_line(data = coords[c("BB", "AX"),], aes(x, y), colour = linecolor, inherit.aes = FALSE) +
  
  scale_x_continuous(expand = c(0, 0), lim = c(0, dim(img)[2])) +
  scale_y_continuous(expand = c(0, 0), lim = c(0, dim(img)[1])) +
  theme_void() +
  theme(aspect.ratio = nrow(img) / ncol(img))

ggdraw() + draw_image('park_street_full.png') + draw_plot(p)

