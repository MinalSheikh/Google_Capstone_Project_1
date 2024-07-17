library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("lubridate")
library(lubridate)
library(dplyr)
install.packages("scales")
library(scales)
install.packages("magrittr")  
library(magrittr)  
install.packages("ggplot2")  # Install the ggplot2 package if not already installed
library(ggplot2) 

# Bike ride trip data from April 2023 till March 2024
df1 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202304-divvy-tripdata.csv")
df2 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202305-divvy-tripdata.csv")
df3 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202306-divvy-tripdata.csv")
df4 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202307-divvy-tripdata.csv")
df5 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202308-divvy-tripdata.csv")
df6 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202309-divvy-tripdata.csv")
df7 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202310-divvy-tripdata.csv")
df8 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202311-divvy-tripdata.csv")
df9 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202312-divvy-tripdata.csv")
df10 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202401-divvy-tripdata.csv")
df11 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202402-divvy-tripdata.csv")
df12 <- read.csv("/Users/Minal/R Projects/Cyclistic_bike_share_analysis/Dataset/202403-divvy-tripdata.csv")  

# Combine 12 data frame into one data frame and remove empty rows and columns
bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
bike_rides <- janitor ::remove_empty(bike_rides, which = c("rows"))
bike_rides <- janitor ::remove_empty(bike_rides, which = c("cols"))
dim(bike_rides)
colnames(bike_rides)
str(bike_rides)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(bike_rides)  #List of column names
nrow(bike_rides)  #How many rows are in data frame?
dim(bike_rides)  #Dimensions of the data frame?
head(bike_rides)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(bike_rides)  #See list of columns and data types (numeric, character, etc)
summary(bike_rides)  #Statistical summary of data. Mainly for numerics

# Remove columns with missing values
bike_rides <- select(bike_rides, -start_station_name, -start_station_id, -end_station_name, -end_station_id)
bike_rides

table(bike_rides$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
bike_rides$date <- as.Date(bike_rides$started_at) #The default format is yyyy-mm-dd
bike_rides$month <- format(as.Date(bike_rides$date), "%m")
bike_rides$day <- format(as.Date(bike_rides$date), "%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date), "%A")

# Add a "ride_length" calculation to bike_rides (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
bike_rides$ride_length <- difftime(bike_rides$ended_at,bike_rides$started_at)
str(bike_rides)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(bike_rides$ride_length)
bike_rides$ride_length <- as.numeric(as.character(bike_rides$ride_length))
is.numeric(bike_rides$ride_length)
str(bike_rides)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
bike_rides_v2 <- bike_rides[!(bike_rides$ride_length<0),]
str(bike_rides_v2)
dim(bike_rides_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(bike_rides_v2$ride_length) #straight average (total ride length / rides)
median(bike_rides_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bike_rides_v2$ride_length) #longest ride
min(bike_rides_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(bike_rides_v2$ride_length)

# Compare count of rides among member and casual users 
executive_summ <- bike_rides_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n()) %>%
  mutate(percentage = round(number_of_rides * 100 / sum(number_of_rides,1)))

# View the result
View(executive_summ)

# plot for User count of member and casual users
ggplot(bike_rides_v2, mapping = aes(x=member_casual, fill = member_casual)) +geom_bar() + labs(title = "User count : Member vs Casual") + theme_minimal()

# Compare members and casual users
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual, FUN = mean)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual, FUN = median)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual, FUN = max)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean)

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)	

# Now, let's Compare bike type used by casual user and member users
 
aggregated_data <- bike_rides_v2 %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(n = n(), average_duration = mean(ride_length, na.rm = TRUE)) %>%
  mutate(percentage = n * 100 / sum(n)) %>%
  arrange(member_casual, rideable_type)
print(aggregated_data)


# Let's visualize the number of rides by user type
install.packages("scales")  # Install the scales package if not already installed
library(scales)             # Load the scales package

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma)


# Let's create a visualization for average duration
bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Let's visualize the number of rides by bike type
bike_rides_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(rideable_type, member_casual) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") 

#Plot to show bike type used by percentage between casual and member users
ggplot(data = aggregated_data, mapping =aes(x=member_casual,y=percentage, fill= rideable_type)) + geom_col() +labs(title = "Bike type used (Casual vs Member)")
#Number Of Rides For Each User Type For One Year 
bike_rides_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(month,member_casual) %>% 
  ggplot(aes(x = month, y = member_casual, group = member_casual)) + geom_line(aes(color = member_casual)) + geom_point() + scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-4)) + scale_x_discrete(labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")) + labs(title = "Number of Rides For Each User Type In One Year", x = "Month", y = "Number of Rides", color = "User Type")

# Average trip duration (avg_ride_length) between casual and member users
# Aggregate the data to calculate the average ride duration for each user type
aggregated_data <- bike_rides_v2 %>%
  group_by(member_casual) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length, na.rm = TRUE)) %>%
  arrange(member_casual)
# Visualization of Average trip duration (avg_ride_length) between casual and member users
ggplot(aggregated_data, aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_col() +
  geom_text(aes(label = average_duration), vjust = -0.5) +  # Add average_duration on top of the bars
  labs(title = "average_duration by User Type", x = "User Type", y = "Average Ride length") +
  theme_minimal()
bike_rides_v2 %>% group_by(member_casual) %>% summarise(average_duration = mean(ride_length)) %>% 
  ggplot(data=bike_rides_v2, mapping = aes(x= member_casual, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title = "Cyclistic's ride trip duration : Casual vs Member")

# Visualization for count of User types
ggplot(bike_rides_v2, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Add counts on top of the bars
  labs(title = "User Count: Member vs Casual", x = "User Type", y = "Count") + 
  theme_minimal()

# Visualization of Number of Rides For Each User Type In One Year

bike_rides_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(month, member_casual) %>%
  ggplot(aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  labs(
    title = "Number of Rides For Each User Type In One Year",
    x = "Month",
    y = "Number of Rides",
    color = "User Type"
  ) 

# Visualization of Average ride duration For Each User Type In One Year

bike_rides_v2 %>%
  group_by(member_casual, month) %>%
  summarise(average_duration = mean(ride_length)) %>%
  arrange(month, member_casual) %>%
  ggplot(aes(x = month, y = average_duration, group = member_casual, color = member_casual)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3)) +
  labs(
    title = "Average duration for Each User Type In One Year",
    x = "Month",
    y = "Average duration",
    color = "User Type"
  ) 

#Total number of rides by members and casual 
ggplot(bike_rides_v2, aes(x = fct_infreq(member_casual)))+ geom_bar(width = 0.5)+ labs(x = NULL, y = "Number of rides", title = "Total rides of Subscribers vs Customers")+ scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$member_casual + bike_rides_v2$day_of_week, FUN = mean)
counts
write.csv(counts, file = 'avg_ride_length.csv')


head(bike_rides)
dim(bike_rides)

