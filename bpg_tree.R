### tree.R ########################################################################################


### PREAMBLE ######################################################################################
library(BoutrosLab.plotting.general);
library(randtoolbox);

options(stringsAsFactors = FALSE);

### PARAMETERS ####################################################################################

height <- 8;
width <- (height/8)*5;


# measure of how dense tree should be
steps <- 130;
ndecor <- 80 


stepheight <- height/steps;

# slope of tree
slope = -(width/2)/height


leaf.colours <- c('#0F3B00', '#2B7314', '#207F00');
bark.colours <- c('#451700', '#592D17');
decoration.colours <- c('#F0BE0B', '#CC1D0C')


# combine for legend
combined.colours <- c(
	leaf.colours, 
	bark.colours, 
	decoration.colours
	);

colour.labels <- c(
	rep('leaves', length(leaf.colours)), 
	rep('bark', length(bark.colours)), 
	rep('ornament', length(decoration.colours))
	)

### tree ##########################################################################################
tree <- function(x) {
	return(height - sign(x)*(2*height/width)*x)
}

### MAKE COMPONENTS ###############################################################################


# leaves, a.    
data = data.frame()
  
for(i in seq(0, height, by=stepheight)) {
	l = -(width/(2*height))*(-height + i)
    n = 40*l
    
    y = runif(n, i, i+stepheight)
    x = runif(n, -l, l)
    colour = sample(
    	leaf.colours, # green colours
    	size = n, 
    	replace = TRUE
    	);
    	

    tempdata = data.frame(x, y, colour)
    
    data = rbind(data, tempdata)
  }

 data$size <- 0.75;

stemh = height/8
stemw = width/6
stemn = 280
    
x = runif(stemn, -stemw/2, stemw/2)
y = runif(stemn, -stemh, 0)
  
stem <- data.frame(
	x,
	y, 
	colour = sample(
		bark.colours, 
		size = stemn,
		replace = TRUE
		),
	size = rep(0.75, stemn)
	);

numbers <- sobol(ndecor, 2);

xdecor <- width*numbers[, 1] - width/2;
ydecor <- height*numbers[, 2]

# xdecor <- runif(ndecor, -width/2, width/2);
# ydecor <- runif(ndecor, 0, height);

colours <- sample(
	decoration.colours, 
	prob = c(0.7, 0.3),
	ndecor, 
	replace = TRUE
	);
size <- rep(1.4, ndecor);
  
decordata = data.frame(xdecor, ydecor, colours, size)
names(decordata) <- c('x', 'y', 'colour', 'size')
  
decorations <- subset(decordata, y < tree(x));
 

### LEGEND ########################################################################################

tree.legend.grob <- draw.key(
	key = list(
		points = list(
			pch = 19, 
			col = combined.colours,
			cex = 1.1
			),
		text = list(
			lab = colour.labels
			), 
		cex.title = 1,
		between = 0.7
		)
	);


combined.data <- rbind(data, stem, decorations);

tree.plot <- create.scatterplot(
	y ~ x, 
	combined.data, 
	# technicalities
	filename = 'tree.png',
	resolution = 300,
	height = 8,
	# points
	col = combined.data$colour,
	cex = combined.data$size,
	# x-axis
	xaxis.cex = 0.8,
	xlab.label = '', 
	# y-axis
	yaxis.cex = 0.8,
	ylab.label = 'Happy Holidays!',
	ylab.cex = 1.4,
	legend = list(
		right = list(
			fun = tree.legend.grob
			)
		)
	);





  