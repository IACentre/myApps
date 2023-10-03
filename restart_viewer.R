# Sample data
x <- 1:10
y <- x^2

# Create a scatter plot
plot(x, y, type = "p", main = "Scatter Plot Example", xlab = "X-axis", ylab = "Y-axis")

# Save the plot to a PNG file (change the filename as needed)
png("scatter_plot.png")

# Print the plot to the active graphics device (in this case, the PNG file)
print(plot(x, y, type = "p", main = "Scatter Plot Example", xlab = "X-axis", ylab = "Y-axis"))

# Close the PNG file
dev.off()
