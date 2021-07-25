#' ---
#' title: "Case Study 1 (Bike-Share)"
#' author: "Shesan G"
#' date: "25/07/2021"
#' output: html_document
#' ---
#' 
#' # Introduction
#' You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data
#' visualizations.
#' 
#' # Ask
#' The objective of this analysis is to discover how casual riders and annual members use the Cyclistic rental bikes different from these insights, our team can design an effective marketing strategy to convert casual riders into annual riders.
#' 
#' ## Key Stakeholders
#' Lily Moreno, The director of marketing.
#' Cyclistic marketing analytics team. 
#' Cyclistic executive team.
#' 
#' # Prepare
#' The Cyclistic historical trip data contains data sets from April 2020 to June 2021, which is available from [Cyclistic trip date] (https://divvy-tripdata.s3.amazonaws.com/index.html). The data has been made available by
#' Motivate International Inc. under this [licence] (https://www.divvybikes.com/data-license-agreement).
#' 
#' The data sets are all in comma-delimited format (.CSV) with identical structures:
#' (ride_id, rideable_type, started_at, ended_at, start_station_name, start_station_id, end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual).
#' 
#' # Process
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
#library(RColorBrewer)
library(scales)

#scipen prints values in numeric values instead of fixed or exponential notation (helpful for graph scales).
options(scipen = 5)

#' 
#' 
#' Merge the files together into a single dataset.
## ----------------------------------------------------------------------------------------------------------------------------
bike_files <- list.files(path = "./Data/", recursive = TRUE, full.name = TRUE, pattern = "*-divvy-tripdata.csv")
bike_merged <- do.call(rbind, lapply(bike_files, read.csv))
print((paste(nrow(bike_merged), "Rows Total")))

#' 
#' Structure of the data.
## ----------------------------------------------------------------------------------------------------------------------------
str(bike_merged)

#' 
#' Removed duplicate rider id's.
## ----------------------------------------------------------------------------------------------------------------------------
bike_unique <- bike_merged[!duplicated(bike_merged$ride_id), ]
print(paste(nrow(bike_merged) - nrow(bike_unique), "Duplicates Rows Removed "))

#' 
#' Removed rows where values were missing. These rows include start_station_id, end_station_id, end_lat, and end_lng.
## ----------------------------------------------------------------------------------------------------------------------------
colSums(is.na(bike_unique))
bike_prepared <- bike_unique %>% 
  drop_na() %>%
  filter(start_station_name != "" | end_station_name != "")
print((paste(nrow(bike_unique) - nrow(bike_prepared), "Missing/Null Rows Removed")))
colSums(is.na(bike_prepared))


#' 
#' Format the structure of the dates.
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared$started_at <- as.POSIXct(bike_prepared$started_at, format = "%Y-%m-%d %H:%M")
bike_prepared$ended_at <- as.POSIXct(bike_prepared$ended_at, format = "%Y-%m-%d %H:%M")


#' 
#' Mutate columns to allow detailed analysis.
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared <- bike_prepared %>%
  mutate(ride_length_sec = as.numeric(difftime(bike_prepared$ended_at,bike_prepared$started_at))) %>%
  mutate(ride_length_min = as.numeric(difftime(bike_prepared$ended_at,bike_prepared$started_at, units = "mins"))) %>%
  mutate(day_of_week_char = weekdays(bike_prepared$started_at)) %>%
  mutate(day_of_week_num = wday((bike_prepared$started_at))) %>%
  mutate(start_hour = hour(bike_prepared$started_at)) %>%
  mutate(start_day = day(bike_prepared$started_at)) %>%
  mutate(start_month = month(bike_prepared$started_at)) %>%
  mutate(start_year = year(bike_prepared$started_at)) %>%
  mutate(start_year_month = paste(start_year, start_month, sep = "-"))


#' 
#' Order the data
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared$day_of_week_char <- ordered(bike_prepared$day_of_week_char, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

bike_prepared$start_year_month <- ordered(bike_prepared$start_year_month, levels=c("2020-4", "2020-5", "2020-6", "2020-7", "2020-8", "2020-9", "2020-10", "2020-11", "2020-12", "2021-1",  "2021-2", "2021-3",  "2021-4",  "2021-5",  "2021-6"))


#' 
#' Remove ride lengths with negative values.
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 <- bike_prepared %>% 
  filter(ride_length_sec > 0)
print((paste(nrow(bike_prepared) - nrow(bike_prepared_2), "Rows Removed, ride length invalid")))


#' 
#' Save the file/Load the file (if required).
## ----------------------------------------------------------------------------------------------------------------------------
#bike_prepared_2 %>% write.csv("./Data/Cleaned_Bike_200204_202106.csv")
#bike_prepared_2 <- read.csv("./Data/Cleaned_Bike_200204_202106.csv")

#' 
#' # Analyse
#' 
#' Summary data of the ride length in minutes. 
## ----------------------------------------------------------------------------------------------------------------------------
summary(bike_prepared_2$ride_length_min)

#' 
## ----------------------------------------------------------------------------------------------------------------------------
setNames(aggregate(bike_prepared_2$ride_length_min ~ bike_prepared_2$member_casual, FUN = mean), 
         c("member/casual", "ride_len_minute_mean"))
setNames(aggregate(bike_prepared_2$ride_length_min ~ bike_prepared_2$member_casual, FUN = median),
         c("member/casual", "ride_len_minute_median"))
setNames(aggregate(bike_prepared_2$ride_length_min ~ bike_prepared_2$member_casual, FUN = max),
         c("member/casual", "ride_len_minute_max"))
setNames(aggregate(bike_prepared_2$ride_length_min ~ bike_prepared_2$member_casual, FUN = min),
         c("member/casual", "ride_len_minute_min"))
setNames(aggregate(bike_prepared_2$ride_length_min ~ bike_prepared_2$member_casual + bike_prepared_2$day_of_week_char, FUN = mean), 
         c("member/casual", "day_of_week", "ride_len_minute_mean"))

#bike_prepared_2 %>% group_by(member_casual, day_of_week_char) %>% summarise_at(vars(ride_length_min), funs(mean, median, max, min))


#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>% 
  ggplot(aes(x = member_casual, fill = member_casual)) + 
  geom_bar() +
  labs(title = "Casual & Member Count")

table(bike_prepared_2$member_casual)

#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  group_by(member_casual, day_of_week_char) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, day_of_week_char) %>%
  ggplot(aes(x = day_of_week_char, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Number of Rides per Weekday")

#Number of rides on weekday (Monday - Friday)
bike_prepared_2 %>%
  group_by(member_casual, day_of_week_char) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  subset(., day_of_week_char != "Saturday" & day_of_week_char != "Sunday") %>%
  summarise(sum(number_of_rides))

#Number of rides on weekend (Saturday, Sunday)
bike_prepared_2 %>%
  group_by(member_casual, day_of_week_char) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  subset(., day_of_week_char != "Monday" & day_of_week_char != "Tuesday" & day_of_week_char != "Wednesday" & day_of_week_char != "Thursday" & day_of_week_char != "Friday") %>%
  summarise(sum(number_of_rides))

  

#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  group_by(member_casual, day_of_week_char) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%
  arrange(member_casual, day_of_week_char) %>%
  ggplot(aes(x = day_of_week_char, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Average Number of Rides per Weekday")


bike_prepared_2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min))
  

#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  group_by(member_casual, start_hour) %>%
  summarise(number_of_riders = n()) %>%
  ggplot(aes(x = start_hour, y = number_of_riders, color = member_casual)) + 
  geom_line() +
  labs(title = "Number of Rides at Start Hour", subtitle = "")

bike_prepared_2 %>%
  group_by(start_hour) %>%
  summarise(number_of_riders = n()) %>%
  arrange(number_of_riders)


#' 
#' 
#' 
#' 
#' 
#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  ggplot(aes(x = start_year_month, fill = member_casual)) +
  geom_bar() + 
  labs(title = "Total Number of riders through 2020-04 to 2021-06") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  group_by(member_casual, start_year_month) %>%
  summarize(number_of_riders = n()) %>%
  ggplot(aes(x = start_year_month, y = number_of_riders, fill = member_casual)) +
  geom_col(position = "dodge") + 
  facet_wrap(~ member_casual) +
  labs(title = "Number of riders through 2020-04 to 2021-06") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


bike_prepared_2 %>%
  group_by(start_year_month) %>%
  summarize(number_of_riders = n()) %>%
  arrange(number_of_riders)


#' 
## ----------------------------------------------------------------------------------------------------------------------------
bike_prepared_2 %>%
  group_by(member_casual, rideable_type, ) %>%
  summarize(number_of_riders = n()) %>%
  ggplot(aes(x = rideable_type, y = number_of_riders, fill = member_casual)) + 
  geom_col(position = "dodge") 


#' 
#' 
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
chi_map <- c(
  left = as.double(min(bike_prepared_2$start_lng, bike_prepared_2$end_lng)),
  right = as.double(max(bike_prepared_2$start_lng, bike_prepared_2$end_lng)),
  bottom = as.double(min(bike_prepared_2$start_lat, bike_prepared_2$end_lat)),
  top = as.double(max(bike_prepared_2$start_lat, bike_prepared_2$end_lat))
)

chicago_stamen <- get_stamenmap(
  bbox = chi_map,
  zoom = 10,
  maptype = "terrain-lines"
)

#Map Plot
ggmap(chicago_stamen) +
  geom_point(data = bike_prepared_2, aes(x = start_lng, y = start_lat, color = member_casual), size = 0.25, alpha = .01) +
  labs(title = "Starting Locations") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  scale_color_manual(values = c("red", "green"))

ggmap(chicago_stamen) +
  geom_point(data = bike_prepared_2, aes(x = end_lng, y = end_lat, color = member_casual), size = .25, alpha = .01) +
  labs(title = "Ending Locations") +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  scale_color_manual(values = c("red", "green"))

#' 
#' 
#' # Share
#' 
#' ### Summary
#' 
#' * 32% more members than casuals in total
#' * 65% more members than casuals in the number of rides on a weekday (Monday to Friday)
#' * 13% more casuals than members in the number of rides on a weekend (Saturday, Sunday)
#' * 90% more members and casuals ride on the weekend compared to weekdays
#' * casuals on average ride 2.73x minutes longer than members
#' * From the hours of 23 (11 pm) to 6 (6am) there are less than 100000 riders that start using the service. The hours of 10 (10am) to  20 (8pm) has 200000+ riders each hour. The most amount of riders occurs at 17 (5pm) at 494204 riders.
#' * From months 4 to 9 (April to Sept) the number of riders is greater than 10 to 3 (October to March). 
#' * In all months there are more members riding than casuals.
#' * Both members and casuals prefer docked bikes, followed by classic bikes. electric bikes are the least popular, but are equally favored to classic bikes by casual users.
#' * The location of stations which riders choose to take bikes out of and return to are located in the downtown district of Chicago.
#' 
#' # Act
#' 
#' ### Hypothesis
#' 
#' * Casual riders prefer to ride on the weekends compared to weekdays unlike the member riders, this could be due to a more casual sightseeing or touring around the city, whereas, members likely use their bikes as a mode of transport for work or school during the weekday. This could also be implied by the duration of the bike rides, as casuals ride 2.73x longer than members meaning they use the bikes to tour compared to getting from point A to point B.
#' * The months with the least amount of riders, October to March, are also the colder months in the Northern Hemisphere. This likely means that riders are less likely to ride in the cold and view biking as more of a summer type of activity.
#' 
#' ### Recommendations
#' 
#' * Promote membership ad campaigns around the downtown district of Chicago as this is where a majority of biking users are located.
#' * Have a tiered bike rental system for duration of rental. ie. It is cheaper to rent for 2 hours compared 1 hour. This will give members a discount as well as, incentive's customers to sign up.
#' * Have promotions for weekend or summer months to get new users to sign up into a membership.
#' * Have promotions featuring docked bikes since all users prefer them compared to the other models.
#' 
#' 
#' 
#' 
#' 
