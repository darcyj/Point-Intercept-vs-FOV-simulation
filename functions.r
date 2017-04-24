# Function to calculate distance between a pair of points.
distance <- function(x1, y1, x2, y2){
	distance <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
	return(distance)}

# Function to generate a random point within a circle
# Returns a vector: x,y
circlepos <- function(plate_radius, circle_radius){
	r <- (plate_radius - circle_radius)
	repeat{
		x <- runif(1, -r, r)
		y <- runif(1, -r, r)
		distance <- sqrt((x*x) + (y*y))
		if(distance <= r){
			xret <- x
			yret <- y
			break
		}
	}
	outvec <- c(x,y)
	return(outvec)}

# Function to make a "plate"
# returns a matrix. first row ([1,]) is x values, second row ([2,]) is 
# y values, corresponding to the locations of microbial phototrophs.
platemaker <- function(cover, plate_radius, colony_radius){
	colony_area <- pi*(colony_radius^2)
	plate_area <- pi*(plate_radius^2)
	n_colonies <- round((plate_area * cover) / colony_area)
	xvals <- c(1:n_colonies)
	yvals <- c(1:n_colonies)
	for(i in 1:n_colonies){
		colony <- circlepos(plate_radius, colony_radius)
		xvals[i] <- colony[1]
		yvals[i] <- colony[2]
	}
	outmat <- mat.or.vec(2,n_colonies)

	rownames(outmat) <- c("xcoord","ycoord")
	outmat[1,] <- xvals
	outmat[2,] <- yvals
	return(outmat) 
}

# Function to simulate point-intercept method on a plate
# n is the number of points tested.
# Returns the ratio of positive points : n
point_intercept <- function(plate, n, plate_radius, colony_radius){
	n_intercepts <- 0
	for(i in 1:n){
		point <- circlepos(plate_radius, 0) #generates our random point on the WHOLE circle
		x1 <- point[1]
		y1 <- point[2]
		intersection <- F
		n_colonies <- length(plate[1,])
		for(j in 1:n_colonies){
			x2 <- plate[1,j]
			y2 <- plate[2,j]
			point_to_center <- distance(x1, y1, x2, y2)
			if(point_to_center <= colony_radius){ #this checks whether the point intersects the colony
				intersection <- T
				break
			}
		}#at this point, if ANY of the colonies in plate intersected our point, intersection should be true.
		if (intersection == T){n_intercepts <- n_intercepts + 1}
	}
	cover <- (n_intercepts / n)
	return(cover)
}
	
# Function to simulate Field-of-view (FOV) method on a plate
# n is the number of FOVs tested.
# Returns the ratio of positive fields : n
FOV <- function(plate, n, plate_radius, colony_radius, FOV_radius){
	n_detections <- 0
	for(i in 1:n){
		FOV <- circlepos(plate_radius, FOV_radius) #generates our random FOV on the smaller circle (whole - FOV radius)
		x1 <- FOV[1]
		y1 <- FOV[2]
		detection <- F
		n_colonies <- length(plate[1,])
		for(j in 1:n_colonies){
			x2 <- plate[1,j]
			y2 <- plate[2,j]
			point_to_center <- distance(x1, y1, x2, y2)
			if(point_to_center <= (colony_radius + FOV_radius)){ #this checks whether the point is within the FOV
				detection <- T
				break}
		}
		if (detection == T){n_detections <- n_detections + 1}
	}
	detect_ratio = n_detections / n
	return(detect_ratio)
}
	
# Function to plot a plate object	
require(plotrix)
plateviewer <- function(plate, colony_radius, plate_radius, maintitle=""){
	xvals <- c(plate_radius, (-1 * plate_radius))
	yvals <- xvals
	plot(yvals, xvals, pch="", xlab="", ylab="", asp=1, main=maintitle)
	for (i in 1:length(plate[1,])){
		draw.circle(plate[1,i], plate[2,i], colony_radius, col="forestgreen", border="forestgreen")	
	}
	draw.circle(0,0,plate_radius)
}	

