########Garbage Collection########
rm( list=ls() ) # remove all existing objects in the environment
gc() 

##Set Working Directory##
setwd("D:/Dev/Google Analytics Cert/Course 8/Capstone Task 1/Data")

########Import packages########
library(ggplot2)
library(tidyverse)
library(lubridate)
library(skimr)
library(waffle)

########Load data########
apr_2020 <- read_csv("Data/202004-divvy-tripdata.csv")
may_2020 <- read_csv("Data/202005-divvy-tripdata.csv")
june_2020 <- read_csv("Data/202006-divvy-tripdata.csv")
july_2020 <- read_csv("Data/202007-divvy-tripdata.csv")
aug_2020 <- read_csv("Data/202008-divvy-tripdata.csv")
sep_2020 <- read_csv("Data/202009-divvy-tripdata.csv")
oct_2020 <- read_csv("Data/202010-divvy-tripdata.csv")
nov_2020 <- read_csv("Data/202011-divvy-tripdata.csv")
dec_2020 <- read_csv("Data/202012-divvy-tripdata.csv")
jan_2021 <- read_csv("Data/202101-divvy-tripdata.csv")
feb_2021 <- read_csv("Data/202102-divvy-tripdata.csv")
mar_2021 <- read_csv("Data/202103-divvy-tripdata.csv")
apr_2021 <- read_csv("Data/202104-divvy-tripdata.csv")

bike_share <- rbind(apr_2020, may_2020, june_2020, july_2020, aug_2020, sep_2020, oct_2020, nov_2020, dec_2020, jan_2021, feb_2021, mar_2021, apr_2021)

########Inspect Data########
head(bike_share)

skim_without_charts(bike_share)

########Convert data types########
bike_share$start_station_id <- as.numeric(bike_share$start_station_id)
bike_share$end_station_id <- as.numeric(bike_share$end_station_id)
#Check
is.numeric(bike_share$start_station_id)
is.numeric(bike_share$end_station_id)

########Check for missing values########
matrix.na <- is.na(bike_share)
pmiss <- colMeans(matrix.na) #proportion of missing values in each column
pmiss
nmiss <- rowMeans(matrix.na) #missing data on each row

########Add new columns for analysis########
bike_share$date <- as.Date(bike_share$started_at)
bike_share$year <- format(as.Date(bike_share$date), "%Y")
bike_share$month <- format(as.Date(bike_share$date), "%m")
bike_share$day <- format(as.Date(bike_share$date), "%d")
bike_share$day_of_week <- format(as.Date(bike_share$date), "%A")
bike_share$hour <- format((bike_share$started_at), "%H")

bike_share$ride_length <- difftime(bike_share$ended_at, bike_share$started_at)
bike_share$ride_length <- as.numeric(bike_share$ride_length)
#Check the calculations
min(bike_share$ride_length)
max(bike_share$ride_length)

########Create new dataset after removing observations########
bike_share_v2 <- bike_share[bike_share$ride_length > 0, ]

########Visualization for membership type distribution########
ride_user_type <- table(bike_share_v2$member_casual)
waffle(ride_user_type/10000, rows=24, xlab="1 square = 10000 rides",
       colors=c("#F8766D", "#00BFC4"))

########Visualization for Number of Rides per day########
bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + 
  geom_col(position="dodge") +
  scale_y_continuous(breaks = seq(0, 620000, 50000)) +
  labs(title="Number of Rides per Day", subtitle="Members vs Casual Users", x="Day of Week", y="Total Rides")

########Visualization for Rides per hour by day of week########
hour_rides <- bike_share_v2 %>% 
  group_by(hour, day_of_week) %>% 
  summarise(total = n())

ggplot(hour_rides, aes(hour, total, color = day_of_week, group = day_of_week)) + 
  geom_line() +
  labs(title="Rides per hour",
       subtitle = "Grouped by Day of Week",
       x = "Hour",
       y = "Total Rides") +
  guides(color = guide_legend("Day of Week")) + 
  scale_y_continuous() +
  scale_color_discrete(labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

########Visualization for rides per hour by membership type########
user_hour <- bike_share_v2 %>% 
  group_by(hour, member_casual) %>% 
  summarise(total = n())

ggplot(user_hour, aes(hour, total, color = member_casual, group = member_casual)) + 
  geom_line() + 
  labs(title = "Rides per hour",
       subtitle = "Grouped by membership type",
       x = "Hour",
       y = "Total Rides") + 
  scale_y_continuous() + 
  theme(legend.title = element_blank())


########Visualization for bike usage per month########
bike_share_v2 %>% 
  group_by(month) %>% 
  summarise(rider_per_month = n()) %>% 
  arrange(month) %>% 
  ggplot(aes(x=month, y=rider_per_month, group=1)) +
  geom_col(aes(fill=rider_per_month)) +
  labs(title="Bike Usage by month", y="Total Rides", x="Month", fill="Total Ride") +
  scale_x_discrete(labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +
  scale_y_continuous(breaks = seq(0, 620000, 50000)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_distiller(palette="YlGnBu", trans = "reverse")

########Summary for ride duration########
summary(bike_share_v2$ride_length)

aggregate(bike_share_v2$ride_length ~ bike_share_v2$member_casual + bike_share_v2$day_of_week, FUN = mean)
#Fix the order of days of the week
bike_share_v2$day_of_week <- ordered(bike_share_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(bike_share_v2$ride_length ~ bike_share_v2$member_casual + bike_share_v2$day_of_week, FUN = mean)


bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

########Visualization for average duration per day of week#########
bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) + 
  geom_col(position="dodge") +
  labs(title="Average Ride Duration", subtitle="Annual Members vs Casual Members", x="Day of Week", y="Average Duration", fill="Membership Type")

########Visualization for average duration by month########
bike_share_v2 %>% 
  group_by(month, member_casual) %>% 
  summarise(total = n(), average_duration = mean(ride_length)) %>% 
  arrange(month) %>% 
  ggplot(aes(x=month, y=average_duration, group=member_casual, color=member_casual)) +
  geom_line() +
  labs(title="Average Duration by Month", y="Average Duration", x="Month", fill="Membership Type") +
  scale_x_discrete(labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +
  scale_y_continuous(breaks = seq(0, 620000, 50000)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_distiller(palette="YlGnBu", trans = "reverse")

#Export summary file for further analysis
counts <- aggregate(bike_share_v2$ride_length ~ bike_share_v2$member_casual + bike_share_v2$day_of_week, FUN = mean)
write.csv(counts, file="D:/Dev/Google Analytics Cert/Course 8/Capstone Task 1/avg_ride_length.csv")

