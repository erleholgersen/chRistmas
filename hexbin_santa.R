### DESCRIPTION ###############################################################
# Draw a hexbin Santa Claus
#

### PREAMBLE ##################################################################
library(ggplot2);
library(dplyr);

### MAIN ######################################################################

## BODY

width <- 5;
height <- 8;
n_body <- 15000;

# polynomial giving boundary of santa
santa_boundary <- function(x, width, height) {
    a <- -4*height/(width^2);
    c <- height;
    
    return( a*x^2 + c);
} 

# sample with rejection region
# not the most computationally efficient, but should work
x <- runif(n_body, -width/2, width/2);
y <- runif(n_body, 0, height);

santa_body <- data.frame(
    x = runif(n_body, -width/2, width/2),
    y = runif(n_body, 0, height)
    );

santa_body <- santa_body %>% filter(y < santa_boundary(x, width, height) );

g <- ggplot(santa_body, aes(x, y)) +
    geom_hex() +
    scale_fill_gradient(low = 'red4', high = 'red2') + 
    coord_fixed();


## HEAD

n_head <-n_body/7;
head_diameter <- 2.5; 

# generate random numbers in square around 0
# we'll move this to our desired height afterwards
santa_head <- data.frame(
    x = runif(n_head, -head_diameter/2, head_diameter/2),
    y = runif(n_head, -head_diameter/2, head_diameter/2)
    );

# filter out to make circle, lift to height of Santa
santa_head <- santa_head %>%
    filter(x^2 + y^2 < (head_diameter/2)^2 ) %>%
    mutate(y = height + y);

g <- g + geom_hex(
        data = santa_head, 
        fill = 'cornsilk2', 
        col = 'beige',
        bins = 40
        );

## BEARD

k_min <- height/3;
k_max <- 0.94*height;
w_beard <- 0.9*head_diameter;
n_beard <- n_body/5;

beard_boundary <- function(x, k_min, k_max, w_beard) {
    c <- k_min;
    a <- 4*(k_max - k_min)/(w_beard^2);
    
    return( a*x^2 + c);
}

santa_beard <- data.frame(
    x = runif(n_beard, -w_beard/2, w_beard/2),
    y = runif(n_beard, k_min, k_max)
    );

santa_beard <- santa_beard %>%
    filter(y >= beard_boundary(x, k_min, k_max, w_beard));

g <- g + geom_hex(
        data = santa_beard, 
        fill = 'white', 
        col = 'gray90', 
        cex = 0.2,
        bins = 50
        );

## HAT

min_y <- 1.05*height;
max_y <- min_y + height/3;
max_width <- 0.93*head_diameter;
n_hat <- n_body/5;

hat_boundary <- function(x, min_y, max_y, max_width) {
    c <- max_y;
    a <- -4*(max_y - min_y)/(max_width^2);
    
    return( a*x^2 + c);
}

santa_hat <- data.frame(
    x = runif(n_hat, -max_width/2, max_width/2),
    y = runif(n_hat, min_y, max_y)
    );

santa_hat <- santa_hat %>%
    filter(y <= hat_boundary(x, min_y, max_y, max_width));

g <- g + 
    geom_hex(data = santa_hat);

# white stuff on hat
n_white <- 1000;
santa_hat_white <- data.frame(
    x = runif(n_white, -0.95*head_diameter/2,  0.95*head_diameter/2),
    y = runif(n_white, min_y, min_y + 0.045*min_y)
    );

g <- g + geom_hex(
        data = santa_hat_white,
        fill = 'white', 
        col = 'gray93',
        cex = 0.5
        );

## FINISHING TOUCHES

eye_offset <- 0.5;
eyes <- data.frame(
    x = c(-eye_offset, eye_offset),
    y = rep(height, 2)
    );

g <- g + geom_point(data = eyes, col = 'darkslategray', cex = 2.7) + 
    xlim( -1.3*width/2, 1.3*width/2) +
    xlab('') + 
    ylab('Happy Holidays!') + 
    labs(fill = 'Merriness') +
    theme_classic();

ggsave(
    'images/hexbin_santa.png',
    g,
    width = 5,
    height = 5
    );
