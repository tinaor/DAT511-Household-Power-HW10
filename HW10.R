# Homework 10 - Analysis of Electric Power Consumption Data
# Tina Or

rm(list=ls())

# check if file exists in directory
household_data <- "household_power_consumption.txt"

if (!file.exists(household_data)) {
  stop("Household data file not found.")
}

# approximate memory requirements for the dataset
file_size_bytes <- file.info(household_data)$size
file_size_mb <- file_size_bytes / (1024^2)
cat("Original file:", round(file_size_mb, 2), "MB\n")

# open a connection and read the header separately
connection <- file(household_data, open = "r")
header_line <- readLines(connection, n = 1, warn = FALSE)

# load the relevant subset of data for the dates 2007-02-01 and 2007-02-03
subset_data <- character(0)

# chatgpt was used to get the code to load only the data for the given dates
### START of chatgpt code ###
repeat {
  # read up to 100,000 lines at a time to limit temporary memory use
  chunk <- readLines(connection, n = 100000, warn = FALSE)
  # an empty chunk means that the end of the file has been reached
  if (length(chunk) == 0) {
    break
  }
  # keep records for the dates 2007-02-01 and 2007-02-03
  keep <- grepl("^(0?1/0?2/2007|0?3/0?2/2007);", chunk)
  subset_data <- c(subset_data, chunk[keep])
}
### END of chatgpt code ###

close(connection)

# create a table with the header line and subset of data
power_data <- read.table(
  text = paste(c(header_line, subset_data), collapse = "\n"),
  header = TRUE,
  sep = ";",
  na.strings = "?",  # replaces all missing values (i.e., ?) with NA
  stringsAsFactors = FALSE,
  check.names = FALSE
)

rm(subset_data, header_line)

# convert Date and Time columns to appropriate DateTime objects
power_data$Date <- as.Date(power_data$Date, format = "%d/%m/%Y")
power_data$DateTime <- as.POSIXct(
  paste(power_data$Date, power_data$Time),
  format = "%Y-%m-%d %H: %M: %S"
  )

# examine the data
head(power_data)
str(power_data)

# remove rows with missing values that may affect plotting
plot_cols <- c(
  "DateTime",
  "Global_active_power",
  "Voltage",
  "Sub_metering_1",
  "Sub_metering_2",
  "Sub_metering_3"
)

power_data <- power_data[complete.cases(power_data[plot_cols]), ]

# create and save plots

# PLOT 1 - Histogram of Global Active Power
jpeg("HW10_plot1.jpg", width = 1000, height = 700, res = 120)

hist(
  power_data$Global_active_power,
  breaks = 20,
  col = "lightblue",
  border = "black",
  main = "Global Active Power",
  xlab = "Global Active Power (kilowatts)",
  ylab = "Frequency"
)

dev.off()

# PLOT 2 - Global Active Power Over Time
jpeg("HW10_plot2.jpg", width = 1200, height = 700, res = 120)

plot(
  power_data$DateTime,
  power_data$Global_active_power,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Global Active Power Over Time",
  xlab = "Date and Time",
  ylab = "Global Active Power (kilowatts)"
)

dev.off()

# PLOT 3 - Energy Sub-Metering Over Time
jpeg("HW10_plot3.jpg", width = 1200, height = 700, res = 120)

plot(
  power_data$DateTime,
  power_data$Sub_metering_1,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Energy Sub-Metering Over Time",
  xlab = "Date and Time",
  ylab = "Sub-Metering"
)

## add the second and third sub-metering lines
lines(power_data$DateTime, power_data$Sub_metering_2, col = "red", lwd = 1)
lines(power_data$DateTime, power_data$Sub_metering_3, col = "blue", lwd = 1)

## add a legend
legend(
  "top",
  inset = c(-0.10, -0.02),
  legend = c("Sub-metering_1", "Sub-metering_2", "Sub-metering_3"),
  col = c("black", "red", "blue"),
  lty = 1,
  lwd = 1,
  cex = 0.7,
  bty = "n"
)

dev.off()

# PART 4 - Voltage Over Time
jpeg("HW10_plot4.jpg", width = 1200, height = 700, res = 120)

plot(
  power_data$DateTime,
  power_data$Voltage,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Voltage Over Time",
  xlab = "Date and Time",
  ylab = "Voltage (volts)"
)

dev.off()

# combine the four plots into a 2x2 grid
jpeg("HW10_combined_plots.jpg", width = 1600, height = 1200, res = 150)

par(mfrow = c(2,2), oma = c(0,0,4,0))

# plot 1 (same as above)
hist(
  power_data$Global_active_power,
  breaks = 20,
  col = "lightblue",
  border = "black",
  main = "Global Active Power",
  xlab = "Global Active Power (kilowatts)",
  ylab = "Frequency"
)

# plot 2 (same as above)
plot(
  power_data$DateTime,
  power_data$Global_active_power,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Global Active Power Over Time",
  xlab = "Date and Time",
  ylab = "Global Active Power (kilowatts)"
)

# plot 3 (same as above)
plot(
  power_data$DateTime,
  power_data$Sub_metering_1,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Energy Sub-Metering Over Time",
  xlab = "Date and Time",
  ylab = "Sub-Metering"
)

## add the second and third sub-metering lines
lines(power_data$DateTime, power_data$Sub_metering_2, col = "red", lwd = 1)
lines(power_data$DateTime, power_data$Sub_metering_3, col = "blue", lwd = 1)

## add a legend
legend(
  "top",
  inset = c(-0.10, -0.02),
  legend = c("Sub-metering_1", "Sub-metering_2", "Sub-metering_3"),
  col = c("black", "red", "blue"),
  lty = 1,
  lwd = 1,
  cex = 0.7,
  bty = "n"
)

# plot 4 (same as above)
plot(
  power_data$DateTime,
  power_data$Voltage,
  type = "l",
  col = "black",
  lwd = 1,
  main = "Voltage Over Time",
  xlab = "Date and Time",
  ylab = "Voltage (volts)"
)

# add overall title for the combined plots
mtext("Household Electric Power Consumption",
      outer = TRUE, side = 3, line = 1.5, cex = 1, font = 2)

dev.off()