library(MASS) # needed for mvrnorm
library(ggplot2) # needed for plots

s_from_radius <- function(radius) {
  return((radius^2)/4)
}

radius_from_area <- function(area) {
  return(sqrt(area/pi))
}

snowballs <- function(lower_radius =3, d = 0.5, density = 100) {
  
}

snowman <- function(lower_radius=3, d = 0.5, density=100) {
  
  # work out snowballs
  s1 = s_from_radius(lower_radius)
  sigma1 = matrix(c(s1, 0, 0, s1), nrow=2, ncol=2)
  mu1 = matrix(c(0, 0))
  area1 = pi*lower_radius^2
  n1 = round(area1*density)
  
  area2 = 0.6*area1
  middle_radius = radius_from_area(area2)
  s2 = s_from_radius(middle_radius)
  sigma2 = matrix(c(s2, 0, 0, s2), nrow=2, ncol=2)
  mu2 = mu1 + c(0, lower_radius + middle_radius + d)
  n2 = round(area2*density)
  
  area3 = 0.6*area2
  upper_radius = radius_from_area(area3)
  s3 = s_from_radius(upper_radius)
  sigma3 = matrix(c(s3, 0, 0, s3), nrow=2, ncol=2)
  mu3 = mu2 + c(0, upper_radius + middle_radius + d)
  n3 = round(area3*density)
  
  lower = mvrnorm(n1, mu1, sigma1)
  middle = mvrnorm(n2, mu2, sigma2)
  upper = mvrnorm(n3, mu3, sigma3)
  
  data = rbind(lower, middle, upper)
  data = as.data.frame(data)
  data$christmas = runif(n1 + n2 + n3)
  
  narms = 70
  armlength = 2*lower_radius
  
  leftx = seq(0, -armlength, length.out = narms)
  lefty = cumsum(sample(c(-0.1, 0.1), replace=TRUE, size=narms))
  lefty = mu2[2] + lefty
  
  
  rightx = seq(0, armlength, length.out = narms)
  # righty = cumsum(rnorm(n=narms, 0, 0.1))
  righty = cumsum(sample(c(-0.1, 0.1), replace=TRUE, size=narms))
  righty = mu2[2] + righty
  
  x = c(leftx, rightx)
  y = c(lefty, righty)

  armdata = data.frame(x, y)
  
  l = 3*lower_radius
  
  # generate arms and snow
  frosty = ggplot(data) + geom_line(data=armdata, aes(x=x, y=y), size=0.7) + 
    geom_point(size=3, aes(x=V1, y=V2, color=christmas)) + 
    scale_color_continuous(low="#FFFFFF", high="#FBFBFB", guide=FALSE) + 
    xlim(-l, l) + coord_fixed()
  
  # add eyes
  eye_d = 0.9*upper_radius
  eye_h = mu3[2] + 0.25*upper_radius 
  eyes = matrix(c(-eye_d/2, eye_d/2, eye_h, eye_h), nrow=2, ncol=2)
  eyes = as.data.frame(eyes)
  
  frosty = frosty + geom_point(data=eyes, aes(V1, V2), size=3)
  
  # add nose
  frosty = frosty + geom_point(x=0, y=mu3[2]-0.3*upper_radius, size=4.5, fill="#DC143C", color="white", shape=21)
  
  
  # add hat
  hatn = 200
  hatheight = 2.5*upper_radius
  hatwidth = 2.2*upper_radius
  y = runif(hatn, min=mu3[2] + upper_radius, max = mu3[2] + upper_radius + hatheight)
  x = runif(hatn, min=-hatwidth/2, max = hatwidth/2)
  hat = data.frame(x, y)
  
  rimn = 100
  rimheight = 0.2*upper_radius
  rimwidth = 2.9*upper_radius
  y = runif(rimn, min=mu3[2] + upper_radius, max = mu3[2] + upper_radius + rimheight)
  x = runif(rimn, min=-rimwidth/2, max = rimwidth/2)
  rim = data.frame(x,y)
  
  hat = rbind(hat, rim)
  
  color = runif(hatn + rimn)
  hat$merry = color
  
  
  frosty = frosty + 
    geom_point(data=hat, aes(x=x, y=y, fill = merry), size=3, shape=21)  + 
    scale_fill_continuous(low="#310C25", high="#A3297A", guide=FALSE)

  return(frosty)
}