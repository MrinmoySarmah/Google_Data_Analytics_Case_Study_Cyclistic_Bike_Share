# Google_Data_Analytics_Case_Study_Cyclistic_Bike_Share
This is a case study for one of Google's Data Analytics Professional Certificate Capstone Projects.

Cyclistic is a fictional bike sharing company based and operating in Chicago.

The executive team is on the lookout for ways to drive growth and is considering a marketing strategy that aims to convert a large portion of casual riders into members.

As a junior data analyst on Cyclistic's marketing analysis team, I've been tasked with presenting to executives my findings and recommendations once I've throughly explored,processed and analyzed any relevent data.
this is the R-code for the cleaning and analysis done on the dataset.

you can find the dataset for this project here : [Cyclistic_bike_share_divvy_tripdata](https://divvy-tripdata.s3.amazonaws.com/index.html)

My case study visuals:
(https://public.tableau.com/views/cyclistic_bike_share_project/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link)


---
title: "Cyclistic Bike Share"
output: html_notebook
---

# Cyclistic dataset : Practice analysis

### Setting up my environment

Notes : Setting up my R environment by loading the 'tidyverse' and other packages:

```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)
```

### Setting my working directory and uploading datasets:

```{r}
getwd()
setwd("/users/MRINMOY/Documents/cyclistic company data/csv files")
q1_2021 <- read_csv("november_cleaned_pivoted.csv")
q2_2021 <- read_csv("december_cleaned_pivoted.csv")
q3_2022 <- read_csv("january_cleaned_pivoted.csv")
q4_2022 <- read_csv("feb_cleaned_pivoted.csv")
q5_2022 <- read_csv("mar_cleaned_pivoted.csv")
q6_2022 <- read_csv("apr_cleaned_pivoted.csv")
```

### Wrangling data and combining into a single file:

```{r}
colnames(q1_2021)
colnames(q2_2021)
colnames(q3_2022)
colnames(q4_2022)
colnames(q5_2022)
colnames(q6_2022)
str(q1_2021)
str(q2_2021)
str(q3_2022)
str(q4_2022)
str(q5_2022)
str(q6_2022)
all_trips <- bind_rows(q1_2021,q2_2021,q3_2022,q4_2022,q5_2022,q6_2022)
all_trips <- all_trips %>%
select(-c(start_lat,start_lng,end_lat,end_lng))
```

### Cleaning up and adding data to prepare for analysis:

```{r}
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
summary(all_trips)
table(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
str(all_trips)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

### convering data type and removing bad data:

```{r}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!
                            (all_trips$start_station_name == "HQ QR"|all_trips$ride_length<0),]
```

### Descriptive analysis on ride_length:

```{r}
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)
```

### Aggregate data:

```{r}
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=mean)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=median)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=max)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual,FUN=min)
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN=mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN=mean)
```

### Visualize number of rides by user type:

```{r}
all_trips_v2%>%
  mutate(weekday = wday(started_at,label = TRUE))%>%
group_by(member_casual,weekday)%>%
  summarise(number_of_rides = n(),average_duration=mean(ride_length))%>%
 arrange(member_casual,weekday) %>%
  ggplot(aes(x = weekday,y = number_of_rides,fill = member_casual)) + geom_col(position = "dodge") + labs(title ="number of rides per day by riders")
```

### Creating visualization for average duration:

```{r}
all_trips_v2%>%
  mutate(weekday = wday(started_at,label = TRUE))%>%
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides = n(),average_duration=mean(ride_length))%>%
  arrange(member_casual,weekday) %>%
  ggplot(aes(x = weekday,y = average_duration,fill = member_casual)) + geom_col(position = "dodge") + labs(title ="Average duration of riders per day")
```

### Exporting summary file for further analysis:

```{r}
counts <- aggregate(all_trips_v2$ride_length~all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN = mean)
write.csv(counts,file = '~/cyclistic company data/csv files/avg_ride_length.csv')
```
