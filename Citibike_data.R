library(readr)
library(tidyverse) #for data import and wrangling
library(dplyr)
library(lubridate) #for date functions
library(ggplot2) #for visualizations

dataset1 <- read_csv("citibike data/202107-divvy-tripdata.csv")
dataset2 <- read_csv("citibike data/202108-divvy-tripdata.csv")
dataset3 <- read_csv("citibike data/202109-divvy-tripdata.csv")
dataset4 <- read_csv("citibike data/202110-divvy-tripdata.csv")
dataset5 <- read_csv("citibike data/202111-divvy-tripdata.csv")
dataset6 <- read_csv("citibike data/202112-divvy-tripdata.csv")
dataset7 <- read_csv("citibike data/202201-divvy-tripdata.csv")
dataset8 <- read_csv("citibike data/202202-divvy-tripdata.csv")
dataset9 <- read_csv("citibike data/202203-divvy-tripdata.csv")
dataset10 <- read_csv("citibike data/202204-divvy-tripdata.csv")
dataset11 <- read_csv("citibike data/202205-divvy-tripdata.csv")
dataset12 <- read_csv("citibike data/202206-divvy-tripdata.csv")


merged_data <- rbind(dataset1,dataset2,dataset3,dataset4,dataset5,dataset6,dataset7,dataset8,dataset9,dataset10,dataset11,dataset12)

View(merged_data)

data_no_dups <- merged_data[!duplicated(merged_data$ride_id),]

#there were no duplicates in the data
print(nrow(data_no_dups))

#reformatting date and time
data_no_dups$started_at <- as.POSIXct(data_no_dups$started_at, "%Y-%m-%d %H:%M:%S")
data_no_dups$ended_at <- as.POSIXct(data_no_dups$ended_at, "%Y-%m-%d %H:%M:%S")

#drop na values from longitude and latitude for further visualization in Tableau
dataset_no_na <- drop_na(data_no_dups, start_lat, end_lat, start_lng, end_lng)
print(nrow(merged_data) - nrow(dataset_no_na))

#creating additional columns for aggregation
View(dataset_no_na)
dataset_no_na$start_date <- as.Date(dataset_no_na$started_at)
dataset_no_na$start_month <- format(as.Date(dataset_no_na$started_at),"%m")
dataset_no_na$start_day <- format(as.Date(dataset_no_na$started_at),"%d")
dataset_no_na$start_hour <- format(as.POSIXct(dataset_no_na$started_at), format = "%H")
dataset_no_na$start_year <- format(as.Date(dataset_no_na$started_at),"%Y")
dataset_no_na$start_day_of_week <- format(as.Date(dataset_no_na$started_at),"%A")


dataset_no_na$end_date <- as.Date(dataset_no_na$ended_at)
dataset_no_na$end_month <- format(as.Date(dataset_no_na$ended_at),"%m")
dataset_no_na$end_day <- format(as.Date(dataset_no_na$ended_at),"%d")
dataset_no_na$end_year <- format(as.Date(dataset_no_na$ended_at),"%Y")
dataset_no_na$end_day_of_week <- format(as.Date(dataset_no_na$ended_at),"%A")

# add a ride length column
dataset_no_na <- mutate(dataset_no_na, ride_length = (ended_at-started_at))
#converting it into numeric and dividing by 60 for minutes
dataset_no_na$ride_length <- as.numeric(as.character(dataset_no_na$ride_length))
dataset_no_na = mutate(dataset_no_na, ride_length = round(ride_length/60,digits=2))


#removing negative ride lengths:
clean_dataset <- dataset_no_na[!(dataset_no_na$ride_length <=0),]

#DESCRIPTIVE ANALYSIS
summary <- clean_dataset %>% 
  summarize(mean(ride_length),median(ride_length),max(ride_length),min(ride_length))


print(summary)
#compare across members and casual users
aggregate(clean_dataset$ride_length ~clean_dataset$member_casual, FUN =mean)
aggregate(clean_dataset$ride_length ~clean_dataset$member_casual, FUN =median)
aggregate(clean_dataset$ride_length ~clean_dataset$member_casual, FUN =max)
aggregate(clean_dataset$ride_length ~clean_dataset$member_casual, FUN =min)

#average ride time by each day for members vs casual users
aggregate(clean_dataset$ride_length ~ clean_dataset$start_day_of_week + clean_dataset$member_casual , FUN =mean)
aggregate(clean_dataset$ride_length ~ clean_dataset$start_month + clean_dataset$member_casual , FUN =mean)

#displaying the days of the week in order
clean_dataset$start_day_of_week <- ordered(clean_dataset$start_day_of_week, levels = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"))
aggregate(clean_dataset$ride_length ~ clean_dataset$start_day_of_week + clean_dataset$member_casual , FUN =mean)

#Analyze ridership data by type and weekday
clean_dataset %>% 
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday)

#Visualizing the number of rides by rider type
clean_dataset %>%
  mutate(weekday=wday(started_at, label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position="dodge")

#Visualizing average duration
clean_dataset %>%
  mutate(weekday =wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides=n(),average_duration=mean(ride_length)) %>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x=weekday,y=average_duration, fill=member_casual)) +
  geom_col(position="dodge")

#visualising number of rides per month
clean_dataset %>%
  group_by(member_casual,start_month,start_day) %>%
  summarise(number_of_rides=n()) %>%
  arrange(member_casual, start_month, start_day) %>%
  ggplot(aes(x=start_day, y=number_of_rides, fill = member_casual)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~start_month)

#visualizing number of rides per day
clean_dataset %>%
  group_by(member_casual,start_day,start_hour) %>%
  summarise(number_of_rides=n()) %>%
  arrange(member_casual, start_day, start_hour) %>%
  ggplot(aes(x=start_hour, y=number_of_rides, fill = member_casual)) +
  geom_bar(position="stack", stat="identity")

View(clean_dataset)