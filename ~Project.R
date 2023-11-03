library(ggplot2)

# import the file in to RStudio
setwd("/Users/kmanv/Downloads")
ProjectData <- read.csv("ProjectData.csv")

#It is helpful to read the various names of the variables
names(ProjectData)

# Loop through each column and calculate the sample mean and sample standard deviation
for (col in names(ProjectData)) {
  # Calculate the sample mean and sample standard deviation for each column
  col_mean <- mean(ProjectData[[col]], na.rm = TRUE)
  col_sd <- sd(ProjectData[[col]], na.rm = TRUE)
  # Print the results for each column
  cat(paste("Category:", col, "\n"))
  cat(paste("Sample Mean:", round(col_mean, 2), "\n"))
  cat(paste("Sample Standard Deviation:", round(col_sd, 2), "\n\n"))
}

# Loop through each column and create a histogram
for (col in names(ProjectData)) {
  # Create a histogram for each column and set the main title as the column name
  hist(ProjectData[[col]], main = col)
}

# Loop over columns of ProjectData and create time plot for each
for (col in colnames(ProjectData)) {
  # Create a new plot
  plot(x = 1:1000, y = ProjectData[, col], type = "l", 
       xlab = "Time", ylab = col, main = paste0("Time plot of ", col))
}

# Create time series plot for all four columns with all curves in one plot
plot(ProjectData[,1], type="l", col="blue", xlab="Time", ylab="Price", main="Time Series Plot of All Four Columns")
lines(ProjectData[,2], col="red")
lines(ProjectData[,3], col="green")
lines(ProjectData[,4], col="purple")
legend("topright", legend=c("ETF", "OIL", "GOLD", "JPM"), col=c("blue", "red", "green", "purple"), lty=1, cex=0.8)
legend("topright", legend=names, col=colors, lty=1, cex=0.8)

# Create a scatterplot between ETF and OIL
plot(ProjectData$ETF, ProjectData$OIL, xlab = "ETF", ylab = "OIL", main = "Scatterplot of ETF vs OIL")


