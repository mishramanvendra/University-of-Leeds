setwd('/Users/manvendramishra/Documents/Semester 1/Statistical theory and Methods/Assignments/Data and code')

# Part 1
# importing libraries
install.packages("dplyr")
library(dplyr)
install.packages('ggplot2')
library(ggplot2)
install.packages("moments")
library(moments)
install.packages("nortest")
library(nortest)
install.packages("writexl")
library(writexl)

load("rehoming.RData")

#  Creating the sample for my user ID
createsample(201911048)

#  Saving the sample in another dataset
save(mysample, file = "Manvendra_Stats_Ass_sample.RData")


# Loading the sample dataset created
load("Manvendra_Stats_Ass_sample.RData")

# ************************************************************************************************
# Part 2



unique_breeds <- rbind(distinct(mysample, Breed))

unique_breeds

#  total number of records#  total numbreedber of records
no_of_records = dim(mysample)[1]

#  Pre-Processing in the dataset (Filtering the NA and unwanted values)
mysample <- mysample[(!is.na(mysample$Rehomed) & mysample$Rehomed != 99999),]

# Total number of records dropped after applying first filter
per_rec_rehomed = dim(mysample)[1]
per_rec_dropped_rehomed = 1 - (per_rec_rehomed/no_of_records)


mysample <- mysample[(!is.na(mysample$Breed)),]

# Total number of records dropped after 2nd filter
per_rec_breed = dim(mysample)[1]
per_rec_dropped_breed = 1 - (per_rec_breed/no_of_records)
per_rec_dropped_breed

# ************************************************************************************************
# Part 3

split_data <- split(mysample, mysample$Breed)

# Initialize an empty list to store summaries
summaries <- list()

# Iterate over each breed to generate summaries
for (breed in names(split_data)) {
  cat("\nSummary for breed:", breed, "\n")
  summaries[[breed]] <- summary(split_data[[breed]][, c("Rehomed", "Health")])
  print(summaries[[breed]])
}

# ************************************************************************************************

# Boxplot for Rehoming Times by Breed
ggplot(mysample, aes(x = Breed, y = Rehomed, fill = Breed)) +
  geom_boxplot() +
  labs(title = "Rehoming Times by Breed", x = "Breed", y = "Rehoming Time (Days)") +
  theme_minimal() + coord_fixed(ratio = 1/50) + theme(plot.title = element_text(hjust = 0.5)) 


# Histogram for Rehoming Times by Breed
ggplot(mysample, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(binwidth = 2, alpha = 0.7, position = "dodge") +
  labs(title = "Distribution of Rehoming Times by Breed", x = "Rehoming Time (Days)", y = "Count") +
  theme_minimal()

# Faceted Boxplots for Rehoming Times by Breed
ggplot(mysample, aes(x = "", y = Rehomed, fill = Breed)) +
  geom_boxplot() +
  facet_wrap(~Breed) +
  labs(title = "Rehoming Times for Each Breed", x = "Breed", y = "Rehoming Time (Days)") +
  theme_minimal()

# Histogram Faceted by Breed
ggplot(mysample, aes(x = Rehomed)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  facet_wrap(~Breed) +
  labs(title = "Histogram of Rehoming Times by Breed", x = "Rehoming Time (Days)", y = "Count") +
  theme_minimal()


# ************************************************************************************************


# Creating density plot to figure out the shape and distribution of the data

# Set up the plotting area to display multiple plots in one frame
par(mfrow = c(1, 2),  # Grid layout for two plots side by side
    mar = c(4, 4, 20, 1))  # Adjust margins for better spacing (reduced top margin)

for (breed in unique_breeds) {
  # Filter data for the current breed
  breed_data <- mysample[mysample$Breed == breed, ]
  
  # Plot histogram with density curve and normal distribution
  hist(breed_data$Rehomed, 
       xlab = paste("Rehomed -", breed), 
       cex.axis = 1, main = "", cex.lab = 1,  
       freq = FALSE, yaxs = "i", ylim = c(0, 0.075), col = "lightblue")
  
  # Add the density curve
  lines(density(breed_data$Rehomed), col = "red", lwd = 2)
  
  # Add the normal distribution curve
  x <- seq(from = min(breed_data$Rehomed), to = max(breed_data$Rehomed), by = 0.1)
  lines(x, dnorm(x, mean = mean(breed_data$Rehomed), sd = sd(breed_data$Rehomed)), lwd = 3, col = "blue")
  
  # Add the legend
  legend("topright",  # Position of the legend
         legend = c("Histogram", "Density Curve", "Normal Distribution"),  # Labels
         col = c("lightblue", "red", "blue"),  # Colors
         lty = c(NA, 1, 1),  # Line types: NA for histogram (no line), solid lines for others
         lwd = c(NA, 2, 3),  # Line widths: NA for histogram, others as specified
         pch = c(15, NA, NA),  # Point characters: square for histogram, none for others
         pt.cex = 2)  # Size of the square (histogram legend marker)
  
  # Add the title closer to the plot
  title(main = paste("Histogram for", breed), line = 1)  # Adjust the `line` value (closer with smaller value)
  
  # Calculate and optionally display skewness in the console (not in the plot)
  skewness_value <- skewness(breed_data$Rehomed)
  message("Breed: ", breed, ", Skewness: ", skewness_value)  # Use message() for console output
  
  # estimating parameters for each breed
  mu <- mean(breed_data$Rehomed)
  message("Breed: ", breed, ", mean value is: ", mu)  # Use message() for console output
  sigma <- sd(breed_data$Rehomed)
  message("Breed: ", breed, ", sigma value is: ", sigma)  # Use message() for console output
  # Create the QQ plot
  qqnorm(breed_data$Rehomed, main = "")
  qqline(breed_data$Rehomed, col = "red")
  
  # Add a title for the QQ plot closer to the chart
  title(main = paste("QQ Plot for", breed), line = 1)  # Adjust the `line` value
  
}

# Reset the plotting parameters to default
par(mfrow = c(1, 1))


# ************************************************************************************************

for (breed in unique_breeds) {
  # Filter data for the current breed
  breed_data <- mysample[mysample$Breed == breed, ]
  print(paste("Shapiro result for breed ",breed," is:"))
  print(shapiro.test(breed_data$Rehomed))
  print(paste("Pearson result for breed ",breed," is:"))
  print(pearson.test(breed_data$Rehomed))}

# ************************************************************************************************

for (breed in unique_breeds) {
  breed_data <- mysample[mysample$Breed == breed, ]
  mu <- mean(breed_data$Rehomed)
  sigma <- sd(breed_data$Rehomed)
  n <-nrow(breed_data)
  SE <- sigma / sqrt(n)
  confidence_level <- 0.95
  print(n)
  # Find the critical value (z-score) for the given confidence level
  z_score <- qnorm((1 + confidence_level) / 2)
  
  # Calculate the margin of error
  margin_of_error <- z_score * SE
  
  # Confidence interval
  lower_bound <- mu - margin_of_error
  upper_bound <- mu + margin_of_error
  
  # Print the results
  cat("The 95% Z-Test Confidence Interval for breed ",breed, " is:\n")
  cat("Lower bound:", round(lower_bound, 2), "\n")
  cat("Upper bound:", round(upper_bound, 2), "\n")  
  
}

# ************************************************************************************************


set.seed(123)  # For reproducibility
par(mfrow = c(1, 3), mar = c(4, 4, 20, 1)) 
for (breed in unique_breeds){
  breed_data <- mysample[mysample$Breed == breed,] # Example dataset
  
  # Define parameters
  n <- 50  # Size of each sample
  num_samples <- 100  # Number of samples to draw
  
  # Initialize vector to store sample means
  sample_means <- numeric(num_samples)
  
  # Calculate sample means for num_samples samples of size n
  for (i in 1:num_samples) {
    sample <- sample(breed_data$Rehomed, size = n, replace = TRUE)
    sample_means[i] <- mean(sample)
  }
  
  # Calculate skewness of the sample means
  sample_mean_skewness <- skewness(sample_means)
  
  # Print results
  cat("Skewness of the sample means:", round(sample_mean_skewness, 3), "\n")

  
  # Plot histogram of sample means
  hist(sample_means, 
       xlab = "Sample Mean", 
       col = "lightblue", 
       border = "black", 
       probability = TRUE,
       ylim = c(0, 0.50))
  
  title(main = paste("Histogram of Sample Means for", breed), line = 1)  # Adjust the `line` value (closer with smaller value)
  
  # Add theoretical normal curve
  curve(dnorm(x, mean = mean(sample_means), sd = sd(sample_means)), 
        col = "blue", lwd = 2, add = TRUE)

}

par(mfrows = c(1,1))
# ************************************************************************************************

# Comparison Test

t.test(x = mysample[mysample$Breed == unique_breeds[1], ]$Rehomed, y = mysample[mysample$Breed == unique_breeds[2], ]$Rehomed, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

t.test(x = mysample[mysample$Breed == unique_breeds[2], ]$Rehomed, y = mysample[mysample$Breed == unique_breeds[3], ]$Rehomed, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

t.test(x = mysample[mysample$Breed == unique_breeds[1], ]$Rehomed, y = mysample[mysample$Breed == unique_breeds[3], ]$Rehomed, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# ************************************************************************************************
