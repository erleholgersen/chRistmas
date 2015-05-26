library(ggplot2)


christmastree <- function(height = 8, width = (height/8)*5, steps=50, size=3, ndecor=40) {
  stepheight = height/steps
  
  tree <- function(x) {
    return(height - sign(x)*(2*height/width)*x)
  }
  
  slope = -(width/2)/height
    
  data = data.frame()
  
  for(i in seq(0, height, by=stepheight)) {
    l = -(width/(2*height))*(-height + i)
    n = 40*l
    
    y = runif(n, i, i+stepheight)
    x = runif(n, -l, l)
    color = runif(n)
    
    tempdata = data.frame(x, y, color)
    
    data = rbind(data, tempdata)
  }
  
  stemh = height/8
  stemw = width/6
  stemn = 200
    
  x = runif(stemn, -stemw/2, stemw/2)
  y = runif(stemn, -stemh, 0)
  
  stem = data.frame(x,y)
  
  
  
  ctree = ggplot() + 
    geom_point(data=stem, aes(x=x, y=y), size=size) + 
    coord_fixed()
  
  ctree = ctree + 
    geom_point(data=data, aes(x=x, y=y, color=color), size=size) + 
    scale_color_continuous(low="#0F2E0F", high="#246B24", guide=FALSE)
  
  xdecor = runif(ndecor, -width/2, width/2)
  ydecor = runif(ndecor, 0, height)
  colors = sample(c("A", "B", "C", "D"), size=ndecor, replace=TRUE)
  
  decordata = data.frame(xdecor, ydecor, colors)
  names(decordata) <- c("x", "y", "color")
  
  decorations = subset(decordata, y < tree(x))
 
  ctree = ctree + geom_point(data=decorations, aes(x=x, y=y), size=4, color="gold")
  
  navy = "#191970"
  
  xdecor = runif(ndecor, -width/2, width/2)
  ydecor = runif(ndecor, 0, height)
  colors = sample(c("A", "B", "C", "D"), size=ndecor, replace=TRUE)
  
  decordata = data.frame(xdecor, ydecor, colors)
  names(decordata) <- c("x", "y", "color")
  
  decorations = subset(decordata, y < tree(x))
  
  ctree = ctree + geom_point(data=decorations, aes(x=x, y=y), size=4, color=navy)
  
  
  return(ctree)
}