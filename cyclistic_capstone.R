library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(skimr)
library(janitor)
library(readxl)
getwd()
trip_apr2020 <- read.csv("D:/google_coursera/capstone/dataset/202004-divvy-tripdata.csv")
trip_may2020 <- read.csv("D:/google_coursera/capstone/dataset/202005-divvy-tripdata.csv")
trip_jun2020 <- read.csv("D:/google_coursera/capstone/dataset/202006-divvy-tripdata.csv")
trip_jul2020 <- read.csv("D:/google_coursera/capstone/dataset/202007-divvy-tripdata.csv")
trip_aug2020 <- read.csv("D:/google_coursera/capstone/dataset/202008-divvy-tripdata.csv")
trip_sep2020 <- read.csv("D:/google_coursera/capstone/dataset/202009-divvy-tripdata.csv")
trip_oct2020 <- read.csv("D:/google_coursera/capstone/dataset/202010-divvy-tripdata.csv")
trip_nov2020 <- read.csv("D:/google_coursera/capstone/dataset/202011-divvy-tripdata.csv")
trip_dec2020 <- read.csv("D:/google_coursera/capstone/dataset/202012-divvy-tripdata.csv")

trip_jan2021 <- read.csv("D:/google_coursera/capstone/dataset/202101-divvy-tripdata.csv")
trip_feb2021 <- read.csv("D:/google_coursera/capstone/dataset/202102-divvy-tripdata.csv")
trip_mar2021 <- read.csv("D:/google_coursera/capstone/dataset/202103-divvy-tripdata.csv")
trip_apr2021 <- read.csv("D:/google_coursera/capstone/dataset/202104-divvy-tripdata.csv")
trip_may2021 <- read.csv("D:/google_coursera/capstone/dataset/202105-divvy-tripdata.csv")
trip_jun2021 <- read.csv("D:/google_coursera/capstone/dataset/202106-divvy-tripdata.csv")
trip_jul2021 <- read.csv("D:/google_coursera/capstone/dataset/202107-divvy-tripdata.csv")
trip_aug2021 <- read.csv("D:/google_coursera/capstone/dataset/202108-divvy-tripdata.csv")
trip_sep2021 <- read.csv("D:/google_coursera/capstone/dataset/202109-divvy-tripdata.csv")

colnames(trip_apr2020)
colnames(trip_may2020)
colnames(trip_jun2020)
colnames(trip_jul2020)
colnames(trip_aug2020)
colnames(trip_sep2020)
colnames(trip_oct2020)
colnames(trip_nov2020)
colnames(trip_dec2020)

colnames(trip_jan2021)
colnames(trip_feb2021)
colnames(trip_mar2021)
colnames(trip_apr2021)
colnames(trip_may2021)
colnames(trip_jun2021)
colnames(trip_jul2021)
colnames(trip_aug2021)
colnames(trip_sep2021)

compare_df_cols(trip_apr2020, trip_may2020, trip_jun2020, trip_jul2020, trip_aug2020, trip_sep2020, trip_oct2020, trip_nov2020,trip_dec2020,
                trip_jan2021, trip_feb2021, trip_mar2021, trip_apr2021, trip_may2021, trip_jun2021, trip_jul2021, trip_aug2021, trip_sep2021)


compare_df_cols(trip_apr2020, trip_may2020, trip_jun2020, trip_jul2020, trip_aug2020, trip_sep2020, trip_oct2020, trip_nov2020,trip_dec2020,
                trip_jan2021, trip_feb2021, trip_mar2021, trip_apr2021, trip_may2021,
                trip_jun2021, trip_jul2021, trip_aug2021, trip_sep2021, return = "mismatch")

trip_apr2020 <- mutate(trip_apr2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_may2020 <- mutate(trip_may2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_jun2020 <- mutate(trip_jun2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_jul2020 <- mutate(trip_jul2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_aug2020 <- mutate(trip_aug2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_sep2020 <- mutate(trip_sep2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_oct2020 <- mutate(trip_oct2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))
trip_nov2020 <- mutate(trip_nov2020,end_station_id=as.character(end_station_id), start_station_id=as.character(start_station_id))

compare_df_cols(trip_apr2020, trip_may2020, trip_jun2020, trip_jul2020, trip_aug2020, trip_sep2020, trip_oct2020, trip_nov2020,trip_dec2020,
                trip_jan2021, trip_feb2021, trip_mar2021, trip_apr2021, trip_may2021,
                trip_jun2021, trip_jul2021, trip_aug2021, trip_sep2021, return = "mismatch")

#combine the data frames

all_trips <- bind_rows(trip_apr2020, trip_may2020, trip_jun2020, trip_jul2020, trip_aug2020, trip_sep2020, trip_oct2020, trip_nov2020,trip_dec2020,
  trip_jan2021, trip_feb2021, trip_mar2021, trip_apr2021, trip_may2021, trip_jun2021, trip_jul2021, trip_aug2021, trip_sep2021)

#remove the unwanted columns
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#rename columns

all_trips <- all_trips %>%
  rename(trip_id=ride_id, ride_type = rideable_type, start_time=started_at, end_time=ended_at, usertype=member_casual)



all_trips$date <- as.Date(all_trips$start_time)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time, units = "mins",)
all_trips$ride_length <- round(all_trips$ride_length,2)


colnames(all_trips)
summary(all_trips)

is.numeric(all_trips$ride_length)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

is.numeric(all_trips$ride_length)


skim(all_trips$ride_length)

unique(all_trips[c("start_station_name")])

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

skim(all_trips_v2)





summary(all_trips_v2$ride_length)

head(all_trips_v2)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
summary(all_trips_v2$ride_length)
write.csv(all_trips_v2,"D:/all_trips_updated.csv")

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday,y=number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday,y=average_duration, fill = member_casual)) + geom_col(position = "dodge")
