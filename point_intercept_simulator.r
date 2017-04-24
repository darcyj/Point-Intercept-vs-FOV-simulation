source("functions.r")

# set up parameters
# pretend all units are cm
n_obs <- 150
plate_rad <- 5
colony_rad <- 0.1
# max_cover is arbitrary, has to be set high enough so that ACTUAL cover
# (which is random) will be high enough to get high values in the sim.
# Can be over 1, that just means there will be overlap. 
max_cover <- 2
# FOV_rad is based on dissecting scope used for FOVs in Schmidt et al. 2012
FOV_rad <- (0.48/2)

# generate a vector of covers for the simulation (low to high)
cover_list <- (1:n_obs)*(max_cover/n_obs) 

# set up output
output_matrix <- mat.or.vec(length(cover_list), 3)
output_matrix[,1] <- cover_list
colnames(output_matrix) <- c("naive%cover", "point-intercept%cover", "FOV_porportion")

# run simulation across different cover values
for(k in 1:(length(output_matrix[,1]))){
	plate_k <- platemaker(output_matrix[k,1], plate_rad, colony_rad)
	output_matrix[k,2] <- point_intercept(plate_k, n_obs, plate_rad, colony_rad)
	output_matrix[k,3] <- FOV(plate_k, n_obs, plate_rad, colony_rad, FOV_rad)
}
output_matrix <- as.data.frame(output_matrix)

pdf("simulation_comparison_plot.pdf")
plot(output_matrix[,3] ~ output_matrix[,2], xlab="PI Cover", ylab="FOV Cover", main=paste("FOV vs PI, colony radius=", colony_rad, "nOBS=", n_obs), pch=20, xlim=c(0,1), ylim=c(0,1))
abline(h=1, col="red")
dev.off()

pdf("plate_example.pdf")
exampleplate <- platemaker(0.25, plate_rad, colony_rad)
example_PI <- point_intercept(exampleplate, n_obs, plate_rad, colony_rad)
platewords <- paste("percent_cover:", round(example_PI * 100, 2))
plateviewer(exampleplate, colony_rad, plate_rad, platewords)
dev.off()




write.table(output_matrix, file.choose(), append = FALSE, quote = FALSE, sep = '\t')





