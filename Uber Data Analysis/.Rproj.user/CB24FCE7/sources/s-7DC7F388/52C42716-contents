#Amar Ram Dev
#1901CS06
#Semester 4
#CS299
#Uber Data Analysis - DATA VISUALIZATION

#Loading packages and the such ################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(pacman, ggplot2, ggthemes, lubridate,
               dplyr, tidyr, DT, scales)
pacman::p_load(wesanderson)

#Putting data into datasets ###################################

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

apr_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-apr14.csv")
may_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-may14.csv")
jun_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-jun14.csv")
jul_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-jul14.csv")
aug_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-aug14.csv")
sep_data <- read.csv("E:/Semester 4 (2nd yr)/CS299/R - Uber Data Analysis/Uber-dataset/uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)

summary(data_2014)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time,
                                  format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time,
                                    format="%m/%d/%Y %H:%M:%S"),
                         format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label=TRUE))
data_2014$year = factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label=TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

# Plotting and Visualization ##################################

# Trips vs Hours (with n without Months) #

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n())

datatable(hour_data)

ggplot(hour_data, aes(hour, Total))+
  geom_bar(stat="identity", fill="turquoise", color="purple")+
  ggtitle("Trips each Hour")+
  theme(legend.position="none")+
  xlab("Hours")+
  ylab("Trips")+
  scale_y_continuous(labels = comma)


month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total=n())

datatable(month_hour)

ggplot(month_hour, aes(hour, Total, fill=month))+
  geom_bar(stat="identity")+
  ggtitle("Trips each Hour - Months inclusive")+
  xlab("Hours")+
  ylab("Trips")+
  labs(fill = "Month")+
  scale_y_continuous(labels=comma)

# Trips vs Days (with n without Months) #

day_data <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total=n())

datatable(day_data)

ggplot(day_data, aes(day, Total))+
  geom_bar(stat="identity", fill="turquoise", color="purple")+
  ggtitle("Trips each Day")+
  xlab("Day")+
  ylab("Trips")+
  theme(legend.position="none")+
  scale_y_continuous(labels=comma)


month_day <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total=n())

datatable(month_day)

ggplot(month_day, aes(month, Total, fill=dayofweek))+
  geom_bar(stat="identity", position="dodge")+
  ggtitle("Trips each Week-Day - Months inclusive")+
  xlab("Day")+
  ylab("Trips")+
  labs(fill = "Day of Week")+
  scale_y_continuous(label=comma)+
  scale_fill_manual(values=colors)

# Trips vs Months #

month_data <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total=n())

datatable(month_data)

ggplot(month_data, aes(month, Total, fill = month))+
  geom_bar(stat="identity")+
  ggtitle("Trips each Month")+
  xlab("Month")+
  ylab("Trips")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=colors)

# Trips vs Bases (with n without months) #

ggplot(data_2014, aes(Base))+
  geom_bar(fill = "turquoise")+
  scale_y_continuous(labels=comma)+
  ggtitle("Trips each Base")+
  ylab("Trips")

ggplot(data_2014, aes(Base, fill = month))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels=comma)+
  ggtitle("Trips each Base - Months inclusive")+
  scale_fill_manual(values = colors)+
  ylab("Trips")+
  labs(fill = "Months")

# HEATMAPS #

# Hour vs Day #

day_vs_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total=n())

datatable(day_vs_hour)

pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot(day_vs_hour, aes(day, hour, fill = Total))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colours = pal)+
  ggtitle("Heat Map: Hour vs Day")+
  xlab("Day")+
  ylab("Hour")+
  labs(fill = "Trips")

# Month vs Day #

month_vs_day <- data_2014 %>%
  group_by(day, month) %>%
  dplyr::summarize(Total=n())

ggplot(month_vs_day, aes(day, month, fill = Total))+
  geom_tile(color="black")+
  scale_fill_gradientn(colours = pal)+
  ggtitle("Heat Map: Month vs Day")+
  xlab("Day")+
  ylab("Month")+
  labs(fill = "Trips")

# Month vs Weekdays #

ggplot(month_day, aes(dayofweek, month, fill = Total))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colours = pal)+
  ggtitle("Heat Map: Month vs Day of Week")+
  xlab("Day of Week")+
  ylab("Month")+
  labs(fill = "Trips")

# Month vs Base #

month_vs_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total=n())

ggplot(month_vs_base, aes(Base, month, fill = Total))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colours = pal)+
  ggtitle("Heat Map: Month vs Base")+
  xlab("Base")+
  ylab("Month")+
  labs(fill = "Trips")

# Weekday vs Base #

dayofweek_vs_base <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(dayofweek_vs_base, aes(Base, dayofweek, fill = Total))+
  geom_tile(color = "black")+
  scale_fill_gradientn(colours = pal)+
  ggtitle("Heat Map: Dy of Week vs Base")+
  xlab("Base")+
  ylab("Day of Week")+
  labs(fill = "Trips")

# GEO PLOT #

#min_lat <- 39.001
#max_lat <- 43.001
#min_lon <- -75.001
#max_lon <- -72.001

min_lat <- 39.50
max_lat <- 42.20
min_lon <- -73.60
max_lon <- -72.05

# without Base #

ggplot(data_2014, aes(x=Lon, y=Lat))+
  geom_point(size=1, colour = "red")+
  scale_x_continuous(limits = c(min_lon, max_lon))+
  scale_y_continuous(limits = c(min_lat, max_lat))+
  theme_map()+
  ggtitle("NYC - Uber Rides - 2014:Apr-Sept")

# with Base #

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base))+
  geom_point(size=1)+
  scale_x_continuous(limits = c(min_lon, max_lon))+
  scale_y_continuous(limits = c(min_lat, max_lat))+
  theme_map()+
  ggtitle("NYC - Uber Rides - 2014:Apr-Sept - Base inclusive")

# CLEAN UP ####################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L


summary(data_2014$day)
