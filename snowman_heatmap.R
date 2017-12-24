### snowman_heatmap.R #########################################################

### PREAMBLE ##################################################################
library(BoutrosLab.plotting.general);
library(MASS);

rm(list = ls(all.names = TRUE));

source('snowman_helper_functions.R');

### PARAMETERS ################################################################

n.rows <- 290;
n.columns <- 160;


# give diameters as proportion of width
diameters <- c(
	0.65*n.columns,
	0.55*n.columns,
	0.40*n.columns
	);


### MAKE GROUND ###############################################################

ground.limit <- n.rows/4;

ground.rows <- ground.limit;
ground.n <- ground.rows*n.columns;

ground.grid <- matrix(
	sample(
		paste0('gray', 1:10),
		size = ground.n, 
		replace = TRUE
		),
	nrow = ground.rows,
	ncol = n.columns
	);


### MAKE SKY ##################################################################

sky.rows <- n.rows - ground.limit;
sky.n <- sky.rows*n.columns;

# sample background colours
sky.grid <- matrix(
	sample(
		# background colours
		c(
			'#000009',
			'#000005',
			'#111161', 
			'#1F1F61'
			# 'cornsilk'
			),
		size = sky.n, 
		replace = TRUE
		),
	nrow = sky.rows,
	ncol = n.columns
	);

# randomly sample stars
star.grid <- matrix(
	sample(
		c(TRUE, FALSE), 
		prob = c(0.015, 0.985), 
		size = sky.n, 
		replace = TRUE
		),
	nrow = sky.rows,
	ncol = n.columns
	);

# remove stars from bottom half of grid (ground)

# set star colour
sky.grid[star.grid] <- 'cornsilk';

### ASSEMBLE BACKGROUND GRID ##################################################

snowman.grid <- rbind(
	ground.grid, 
	sky.grid
	);

### ADD SNOWMAN ###############################################################

radii <- diameters/2;

middle.y.coordinate <- n.columns/2 + radii[1] + 0.6*radii[2];
top.y.coordinate <- n.columns/2 + sum(radii[c(1, 3)]) + 1.35*radii[2];

centres <- list(
	c(n.columns/2, n.columns/2),
	c(middle.y.coordinate, n.columns/2),
	c(top.y.coordinate, n.columns/2)
	);

for(k in 1:length(centres)) {
	radius <- radii[k];
	centre <- centres[[k]];

	# calculate distance to centre for each point on grid
	distance.matrix <- matrix(
		mapply(
			function(i, j, centre) {
				distance <- dist(
					rbind(
						c(i, j),
						centre
						)
					);

				return(distance);

				},
			row(snowman.grid),
			col(snowman.grid), 
			MoreArgs = list(
				centre = centre
				)
			),
		nrow = nrow(snowman.grid)
		);

	fuzzy.border <- abs(distance.matrix - radius) < 5;

	distance.matrix[ fuzzy.border ] <- distance.matrix[ fuzzy.border] + rnorm(sum(distance.matrix), sd = 1.2 );


	snowman.grid[distance.matrix <= radius ] <- sample(
		paste0('gray', 95:100),
		size = sum(distance.matrix <= radius),
		replace = TRUE
		);
}


### ADD SNOWMAN ACCESSORIES ###################################################

# eyes
eye.radius <- min(diameters)/17;
eye.offset <- 2.5*eye.radius;

eye.centres <- list(
	c(top.y.coordinate + eye.offset/2, n.columns/2 - eye.offset),
	c(top.y.coordinate + eye.offset/2, n.columns/2 + eye.offset)
	);

for(k in 1:length(eye.centres)) {
	centre <- eye.centres[[k]];

	# calculate distance to centre for each point on grid
	distance.matrix <- matrix(
		mapply(
			function(i, j, centre) {
				distance <- dist(
					rbind(
						c(i, j),
						centre
						)
					);

				return(distance);

				},
			row(snowman.grid),
			col(snowman.grid), 
			MoreArgs = list(
				centre = centre
				)
			),
		nrow = nrow(snowman.grid)
		);


	snowman.grid[distance.matrix <= eye.radius ] <- 'black';
}

# nose
nose.centre <- c(eye.centres[[1]][1] - 0.2*diameters[3], n.columns/2);
nose.radius <- min(diameters)/11;

distance.matrix <- matrix(
	mapply(
		function(i, j, nose.centre) {
			distance <- dist(
				rbind(
					c(i, j),
					nose.centre
					)
				);

			return(distance);

			},
		row(snowman.grid),
		col(snowman.grid), 
		MoreArgs = list(
			nose.centre = nose.centre
			)
		),
	nrow = nrow(snowman.grid)
	);

snowman.grid[distance.matrix <= nose.radius ] <- 'darkorange2';


# buttons
button.radius <- min(diameters)/16;
button.offset <- 6*button.radius;

button.centres <- list(
	c(middle.y.coordinate + button.offset, n.columns/2),
	c(middle.y.coordinate, n.columns/2),
	c(middle.y.coordinate - button.offset, n.columns/2)
	);

for(k in 1:length(button.centres)) {
	centre <- button.centres[[k]];

	# calculate distance to centre for each point on grid
	distance.matrix <- matrix(
		mapply(
			function(i, j, centre) {
				distance <- dist(
					rbind(
						c(i, j),
						centre
						),
					method = 'manhattan'
					);

				return(distance);

				},
			row(snowman.grid),
			col(snowman.grid), 
			MoreArgs = list(
				centre = centre
				)
			),
		nrow = nrow(snowman.grid)
		);


	snowman.grid[distance.matrix <= button.radius ] <- '#4f4847';
}


### MAKE PLOT #################################################################

centres.y <- sapply(
	centres, 
	function(x) x[1]
	);

bottom.y <- centres.y - diameters/2;
top.y <- centres.y + diameters/2; 


colour.scheme <- unique(as.vector(snowman.grid));

frosty.data <- matrix(
	as.numeric(
		factor(
			snowman.grid,
			levels = colour.scheme
			)
		),
	nrow = nrow(snowman.grid)
	);


row.hclust.object <- hclust( dist( 1:nrow(frosty.data)) );

row.dendrogram <- as.dendrogram(row.hclust.object);
row.dendrogram <- reorder(
	row.dendrogram, 
	wts = 1:nrow(frosty.data), 
	agglo.FUN = mean
	);


column.indices <- 1:ncol(frosty.data);

n <- 30;
random.indices <- sample(column.indices, size = n);
column.indices[random.indices] <- column.indices[random.indices] + runif(n, min = -0.5, max = 0.5);

col.hclust.object <- hclust( dist( column.indices), method = 'ward.D2' );

col.dendrogram <- as.dendrogram(col.hclust.object);
col.dendrogram <- reorder(
	col.dendrogram, 
	wts = 1:ncol(frosty.data), 
	agglo.FUN = mean
	);


create.heatmap(
	t(frosty.data),
	# turn off clustering
	filename = 'snowman.png',
	height = 4.8,
	width = 3,
	clustering.method = 'none',
	row.dendrogram = row.dendrogram,
	col.dendrogram = col.dendrogram,
	colour.scheme = colour.scheme,
	colourkey.cex = 1,
	total.colours = length(colour.scheme) + 1,
	ylab.label = 'Happy Holidays!',
	ylab.cex = 1.3,
	colourkey.labels.at = c(5, 13, 19),
	colourkey.labels = c('ground', 'sky', 'snow'),
	# stratified.clusters.rows = stratified.row.clusters
	);

frosty <- create.heatmap(
	t(frosty.data),
	# turn off clustering
	filename = 'snømann.png',
	height = 4.8,
	width = 3,
	clustering.method = 'none',
	row.dendrogram = row.dendrogram,
	col.dendrogram = col.dendrogram,
	colour.scheme = colour.scheme,
	colourkey.cex = 1,
	total.colours = length(colour.scheme) + 1,
	ylab.label = 'God jul!',
	ylab.cex = 1.3,
	colourkey.labels.at = c(5, 13, 19),
	colourkey.labels = c('bakke', 'himmel', 'snø'),
	# stratified.clusters.rows = stratified.row.clusters
	);











