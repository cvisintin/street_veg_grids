library(ggplot2)
library(grid)
library(jpeg)
library(ggimage)

img <- readJPEG("park_street_full.jpg")

img_coords <- data.frame(x = c(0, dim(img)[2], 0, dim(img)[2]), y = c(0, 0, dim(img)[1], dim(img)[1]))

plot(as.raster(img))

graphics::lines(coords["L", ], coords["F", ], col = "red")
points()

img2 <- rasterGrob(img , interpolate = TRUE)

df1 <- data.frame(x = dim(img)[2] / 2,
                  y = dim(img)[1] / 2,
                  image = "park_street_full.png")

ggplot(df1, aes(x, y)) + 
  geom_image(aes(image = image), size = 2) +
  geom_point(data = data.frame("x" = c(0, dim(img)[2], 0, dim(img)[2]), "y" = c(0, 0, dim(img)[1], dim(img)[1])), mapping = aes(x = x, y = y), colour = "red") +
  theme_void() +
  theme(aspect.ratio = nrow(img) / ncol(img),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  



p <- ggplot(mtcars, aes(x = cyl, y = mpg)) + 
  geom_image(aes(image = "park_street_full.jpg")) +
  geom_point() +
  theme(aspect.ratio = nrow(img) / ncol(img),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


grid.draw(gList(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")), 
                ggplotGrob(p)))


plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

plot_jpeg('park_street_full.jpg')
