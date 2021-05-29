Bike Share Exploratory Data Analysis
================
Billy Sanusi

## About the project

This project is a part of the Google Data Analytics certification
capstone project. The dataset we are exploring is from a fictitious
bike-share company based in Chicago, IL called Cyclistic.

Cyclistic has a fleet of 5,824 bicycles that are geotracked and locked
into a network of 692 stations across Chicago. The bikes can be unlocked
from one station and returned to any other station in the system
anytime. The bike-share company offers several pricing plans which
include: single-ride passes, full-day passes, and annual memberships.
Customers who purchase single-ride or full-day passes are referred to as
casual riders. Customers who purchase annual membership are Cyclistic
members.

The purpose of this project is to identify trends from historical data
understand how annual members and casual riders use Cyclistic bikes
differently.

Here, we will take a look at the past 12 months of Cyclistic’s customers
data. From April 2020 until April 2021.

First, we will start by loading a few packages.

``` r
library(ggplot2)
library(tidyverse)
library(lubridate)
library(skimr)
library(waffle)
```

## Loading the data

Next, we will load all the data from the past 12 months and merge them
into a single dataframe

``` r
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
```

## First glance at the Cyclistic Data

We have 3,826,978 rows of data of Cyclistic bike trips from the past
year.

We will inspect the column names and the data types of each to make sure
they are the right data type.

``` r
head(bike_share)
```

    ## # A tibble: 6 x 13
    ##   ride_id rideable_type started_at          ended_at            start_station_n~
    ##   <chr>   <chr>         <dttm>              <dttm>              <chr>           
    ## 1 A847FA~ docked_bike   2020-04-26 17:45:14 2020-04-26 18:12:03 Eckhart Park    
    ## 2 5405B8~ docked_bike   2020-04-17 17:08:54 2020-04-17 17:17:03 Drake Ave & Ful~
    ## 3 5DD24A~ docked_bike   2020-04-01 17:54:13 2020-04-01 18:08:36 McClurg Ct & Er~
    ## 4 2A59BB~ docked_bike   2020-04-07 12:50:19 2020-04-07 13:02:31 California Ave ~
    ## 5 27AD30~ docked_bike   2020-04-18 10:22:59 2020-04-18 11:15:54 Rush St & Hubba~
    ## 6 356216~ docked_bike   2020-04-30 17:55:47 2020-04-30 18:01:11 Mies van der Ro~
    ## # ... with 8 more variables: start_station_id <chr>, end_station_name <chr>,
    ## #   end_station_id <chr>, start_lat <dbl>, start_lng <dbl>, end_lat <dbl>,
    ## #   end_lng <dbl>, member_casual <chr>

``` r
skim_without_charts(bike_share)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | bike\_share |
| Number of rows                                   | 3826978     |
| Number of columns                                | 13          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 7           |
| numeric                                          | 4           |
| POSIXct                                          | 2           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim\_variable       | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| ride\_id             |          0 |           1.00 |  16 |  16 |     0 |   3826769 |          0 |
| rideable\_type       |          0 |           1.00 |  11 |  13 |     0 |         3 |          0 |
| start\_station\_name |     148231 |           0.96 |  10 |  53 |     0 |       713 |          0 |
| start\_station\_id   |     148857 |           0.96 |   1 |  35 |     0 |      1265 |          0 |
| end\_station\_name   |     171416 |           0.96 |  10 |  53 |     0 |       712 |          0 |
| end\_station\_id     |     171877 |           0.96 |   1 |  35 |     0 |      1266 |          0 |
| member\_casual       |          0 |           1.00 |   6 |   6 |     0 |         2 |          0 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |   sd |     p0 |    p25 |    p50 |    p75 |   p100 |
|:---------------|-----------:|---------------:|-------:|-----:|-------:|-------:|-------:|-------:|-------:|
| start\_lat     |          0 |              1 |  41.90 | 0.04 |  41.64 |  41.88 |  41.90 |  41.93 |  42.08 |
| start\_lng     |          0 |              1 | -87.64 | 0.03 | -87.87 | -87.66 | -87.64 | -87.63 | -87.52 |
| end\_lat       |       5005 |              1 |  41.90 | 0.04 |  41.54 |  41.88 |  41.90 |  41.93 |  42.16 |
| end\_lng       |       5005 |              1 | -87.65 | 0.03 | -88.07 | -87.66 | -87.64 | -87.63 | -87.44 |

**Variable type: POSIXct**

| skim\_variable | n\_missing | complete\_rate | min                 | max                 | median              | n\_unique |
|:---------------|-----------:|---------------:|:--------------------|:--------------------|:--------------------|----------:|
| started\_at    |          0 |              1 | 2020-04-01 00:00:30 | 2021-04-30 23:59:53 | 2020-09-06 18:00:14 |   3338950 |
| ended\_at      |          0 |              1 | 2020-04-01 00:10:45 | 2021-05-05 22:14:39 | 2020-09-06 18:30:35 |   3326398 |

## Cleaning the dataset

First, we will start by changing the columns into the right data types.

We realize that columns start\_station\_id and end\_station\_id were
formatted as characters, we want them to be numeric.

``` r
bike_share$start_station_id <- as.numeric(bike_share$start_station_id)
bike_share$end_station_id <- as.numeric(bike_share$end_station_id)
#Check
is.numeric(bike_share$start_station_id)
```

    ## [1] TRUE

``` r
is.numeric(bike_share$end_station_id)
```

    ## [1] TRUE

Next, we will check for missing values.

We have several missing values under the start\_station name,
start\_station\_id, end\_station\_name, end\_station\_id, end\_lat, and
end\_lng. We will not be removing or imputing values for this project.

``` r
matrix.na <- is.na(bike_share)
pmiss <- colMeans(matrix.na) #proportion of missing values in each column
pmiss
```

    ##            ride_id      rideable_type         started_at           ended_at 
    ##         0.00000000         0.00000000         0.00000000         0.00000000 
    ## start_station_name   start_station_id   end_station_name     end_station_id 
    ##         0.03873317         0.15371868         0.04479148         0.15807381 
    ##          start_lat          start_lng            end_lat            end_lng 
    ##         0.00000000         0.00000000         0.00130782         0.00130782 
    ##      member_casual 
    ##         0.00000000

``` r
nmiss <- rowMeans(matrix.na) #missing data on each row
```

We will add several new columns for date, year, month, day,
day\_of\_week, hour, and ride\_length to help us with our analysis.

``` r
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
```

    ## [1] -1742998

``` r
max(bike_share$ride_length)
```

    ## [1] 3523202

As we can see, our new ride\_length column includes negative values. We
will remove these values for this project because a negative value for
ride\_length might be due to an error.

We will create a new version of the dataframe since we will be removing
negative values from the dataframe.

``` r
bike_share_v2 <- bike_share[bike_share$ride_length > 0, ]
```

## Data Exploration and Visualization

Now, let’s explore the data.

We will start by looking at the distribution of membership between
annual members and casual members.

We can see here that the membership distribution is pretty even with
more annual members than casual members.

``` r
ride_user_type <- table(bike_share_v2$member_casual)

waffle(ride_user_type/10000, rows=24, xlab="1 square = 10000 rides",
       colors=c("#F8766D", "#00BFC4"))
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Let’s compare the number of rides for each day of the week between
annual members and casual members.

On weekdays, we can see that the majority of riders are annual members
but the number of casual members increases greatly on weekends. With
more casual members on a sunday compared to annual members.

``` r
bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) + 
  geom_col(position="dodge") +
  scale_y_continuous(breaks = seq(0, 620000, 50000)) +
  labs(title="Number of Rides per Day", subtitle="Members vs Casual Users", x="Day of Week", y="Total Rides")
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Here we created a line chart for rides per hour while grouping them by
day of the week.

The line chart suggested that on weekdays the number of rides increases
during work travel hours (6-8am, 4-6pm) while on weekends the chart
represents a normal distribution of bike usage peaking in the afternoon.

``` r
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
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Now, a similar visualization was created but grouping them by membership
type.

Here, we can see that the annual member line mirrors the lines that
represent weekdays on the previous chart while the casual member line
mirrors the lines that represent weekends from the previous chart.

This could suggest that customers with annual membership are mostly
employees who uses the bike sharing service for work commute and casual
members tend to use the bike sharing service mostly for leisure weekend
rides.

``` r
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
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Let’s look at how bike riders differ by grouping them into different
months of the year.

The graph shows us that bike usage increases during warmer months like
June, July, August, and September. But notice the spike in bike usage on
April, after doing some research on holidays throughout the year, the
spike could be the result of many schools and colleges having its spring
break during the month of April.

``` r
##Histogram for bike usage by month
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
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

To have a better understanding on how annual members and casual riders
use Cyclistic bikes differently, let’s compare the average ride length
for annual members versus casual members.

``` r
summary(bike_share_v2$ride_length)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1     471     864    1657    1587 3523202

``` r
aggregate(bike_share_v2$ride_length ~ bike_share_v2$member_casual + bike_share_v2$day_of_week, FUN = mean)
```

    ##    bike_share_v2$member_casual bike_share_v2$day_of_week
    ## 1                       casual                    Friday
    ## 2                       member                    Friday
    ## 3                       casual                    Monday
    ## 4                       member                    Monday
    ## 5                       casual                  Saturday
    ## 6                       member                  Saturday
    ## 7                       casual                    Sunday
    ## 8                       member                    Sunday
    ## 9                       casual                  Thursday
    ## 10                      member                  Thursday
    ## 11                      casual                   Tuesday
    ## 12                      member                   Tuesday
    ## 13                      casual                 Wednesday
    ## 14                      member                 Wednesday
    ##    bike_share_v2$ride_length
    ## 1                  2556.8166
    ## 2                   938.6098
    ## 3                  2654.2386
    ## 4                   915.2497
    ## 5                  2770.3937
    ## 6                  1061.9097
    ## 7                  3000.3448
    ## 8                  1085.5505
    ## 9                  2504.0088
    ## 10                  902.6322
    ## 11                 2422.4125
    ## 12                  904.0318
    ## 13                 2410.0592
    ## 14                  910.3355

``` r
#Fix the order of days of the week
bike_share_v2$day_of_week <- ordered(bike_share_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(bike_share_v2$ride_length ~ bike_share_v2$member_casual + bike_share_v2$day_of_week, FUN = mean)
```

    ##    bike_share_v2$member_casual bike_share_v2$day_of_week
    ## 1                       casual                    Sunday
    ## 2                       member                    Sunday
    ## 3                       casual                    Monday
    ## 4                       member                    Monday
    ## 5                       casual                   Tuesday
    ## 6                       member                   Tuesday
    ## 7                       casual                 Wednesday
    ## 8                       member                 Wednesday
    ## 9                       casual                  Thursday
    ## 10                      member                  Thursday
    ## 11                      casual                    Friday
    ## 12                      member                    Friday
    ## 13                      casual                  Saturday
    ## 14                      member                  Saturday
    ##    bike_share_v2$ride_length
    ## 1                  3000.3448
    ## 2                  1085.5505
    ## 3                  2654.2386
    ## 4                   915.2497
    ## 5                  2422.4125
    ## 6                   904.0318
    ## 7                  2410.0592
    ## 8                   910.3355
    ## 9                  2504.0088
    ## 10                  902.6322
    ## 11                 2556.8166
    ## 12                  938.6098
    ## 13                 2770.3937
    ## 14                 1061.9097

Grouping the data by membership type and day of week, then sorting them.
We can clearly see that the average duration for each bike ride is
higher for casual members compared to annual members.

``` r
bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```

    ## # A tibble: 14 x 4
    ## # Groups:   member_casual [2]
    ##    member_casual weekday number_of_rides average_duration
    ##    <chr>         <ord>             <int>            <dbl>
    ##  1 casual        Sun              287437            3000.
    ##  2 casual        Mon              167179            2654.
    ##  3 casual        Tue              165628            2422.
    ##  4 casual        Wed              170370            2410.
    ##  5 casual        Thu              178723            2504.
    ##  6 casual        Fri              231450            2557.
    ##  7 casual        Sat              362762            2770.
    ##  8 member        Sun              290426            1086.
    ##  9 member        Mon              295039             915.
    ## 10 member        Tue              316181             904.
    ## 11 member        Wed              330417             910.
    ## 12 member        Thu              328506             903.
    ## 13 member        Fri              342152             939.
    ## 14 member        Sat              349732            1062.

We can see that the average duration for each ride is higher for casual
members compared to annual members and looking at the visualization we
created, the average ride duration is longer during the weekends.

``` r
#Create visualization for average duration per day of week
bike_share_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) + 
  geom_col(position="dodge") +
  labs(title="Average Ride Duration", subtitle="Annual Members vs Casual Members", x="Day of Week", y="Average Duration", fill="Membership Type")
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Grouping the average duration by month shows that ride duration
increases during warmer months for casual members similar to the
increase in number of rides during those months. While the ride duration
for annual members stayed pretty much the same throughout the year.

``` r
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
```

![](Case_study_1_markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Conclusion

After exploring the data and creating different visualizations, we found
that the majority of bike riders are annual members where users use the
bikes for commute to/from work while casual members use the bikes for
longer and leisure rides during the weekends. While annual members and
casual members tend to use the bikes for different reasons, it can be
concluded that the average duration of bike rides increases during
warmer months.

## Further Exploration and Analysis

With more data we could explore more interesting things in the future
such as:

1.  Comparisons between Cyclistic and other bike sharing companies.
2.  The effect of weather conditions on bike usage throughout the year.
3.  Predictive analysis to help with growing demands on warmer months.

#### Data Source

<https://divvy-tripdata.s3.amazonaws.com/index.html>
