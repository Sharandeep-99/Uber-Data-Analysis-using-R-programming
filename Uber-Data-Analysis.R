knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)

# Other library imports you might need (e.g., lubridate, dplyr, tidyr, DT, scales)

# Define color palette
colors <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# Read individual month data files (assuming separate files for April-September)
apr_data <- read.csv(file.choose())
may_data <- read.csv(file.choose())
jun_data <- read.csv(file.choose())
jul_data <- read.csv(file.choose())
aug_data <- read.csv(file.choose())
sep_data <- read.csv(file.choose())

# Combine monthly data into a single data frame
data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

# Data cleaning and transformation steps (add your specific cleaning logic here)

# ... (your data cleaning code)

# Extract hour, day, month, day of week, etc. from the datetime variable (assuming 'Date.Time' is the datetime column)
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

# Analysis and visualizations

# Total trips per hour
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n())

ggplot(hour_data, aes(hour, Total)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# Trips by hour and month
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# Total trips per day
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n())

ggplot(day_group, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# Trips by day and month
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month))

# Trips by month (assuming 'month' is a factor)
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n())

ggplot(month_group, aes(month, Total, fill = month)) +
  geom_bar(stat = "identity") +  # You can simplify to just geom_bar()
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

# Trips by day of week and month
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips by Day of Week and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

# Heatmap - Trips by Hour and Day
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

# Heatmap - Trips by Month and Day
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

# Heatmap - Trips by Day of Week and Month
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

# Assuming 'Base' exists in your data
month_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n())

day0fweek_bases <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

# Heatmap - Trips by Month and Base
ggplot(month_base, aes(x = Base, y = month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

# Heatmap - Trips by Base and Day of Week
ggplot(day0fweek_bases, aes(x = Base, y = dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


library(ggplot2)
library(ggthemes)  # Add this line
# ... other libraries

knitr::opts_chunk$set(echo = TRUE)

# ... (your data cleaning and transformation code)

# ... (your previous visualization code)

# Map of Uber rides based on latitude and longitude
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x = Lon, y = Lat)) +
  geom_point(size = 1, color = "blue") +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x = Lon, y = Lat, color = Base)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
