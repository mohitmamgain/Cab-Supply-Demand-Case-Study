# SET WORKING DIRECTORY
# load the csv file into dataframe
uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

# to find number of NA values in columns.
sum(is.na(uber_data$Request.timestamp)) # No NA values
sum(is.na(uber_data$Drop.timestamp)) # 3914 Na values because trip are either cancelled ot cars not available
sum(is.na(uber_data$Driver.id)) # 2650 NA values because of cars not available
sum(is.na(uber_data$Request.id)) # NO Na values
sum(is.na(uber_data$Pickup.point)) # No NA values
sum(is.na(uber_data$Status)) # No NA values

# convert into the lowecase for ease of analysis.(data cleaning)
uber_data$Pickup.point <- factor(tolower(uber_data$Pickup.point))
uber_data$Status <- factor(tolower(uber_data$Status))

# to check wheather any row is empty or not
sapply(uber_data, function(x) length(which(x == "")))

# repalce / by - to make dates in single format.
uber_data$Request.timestamp <- gsub("/", "-", uber_data$Request.timestamp)
uber_data$Drop.timestamp <- gsub("/", "-", uber_data$Drop.timestamp)
# use of lubridate package for converting date and time in one single format.
library(lubridate)
uber_data$Request.timestamp <- parse_date_time(uber_data$Request.timestamp, orders = c("%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M"))
uber_data$Drop.timestamp <- parse_date_time(uber_data$Drop.timestamp, orders = c("%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M"))

# Although we can extract hours, minute, seconds, date, month and year but hours is coming only in analysis.
# extracting hours from date-time column for useful analysis
uber_data$Request_time_hour <- format(uber_data$Request.timestamp, "%H")

# seperate date and time in differnet column
library(tidyr)
uber_data <- separate(uber_data, Request.timestamp, into=c("Request_date", "Request_time"), sep=" ")
uber_data <- separate(uber_data , Drop.timestamp, into=c("Drop_date", "Drop_time"), sep=" ")

# making a new column and arranging hours in different day format i.e early morning, morning, noon, evening and night.

uber_data$Request_time_hour <- as.numeric(as.character(uber_data$Request_time_hour))
uber_data$day_period[((uber_data$Request_time_hour >= 00) & 
                              (uber_data$Request_time_hour <= 04))] <- c("early_morning")
 
uber_data$day_period[((uber_data$Request_time_hour >= 05) & 
                              (uber_data$Request_time_hour <= 10))] <- c("morning")
 
uber_data$day_period[((uber_data$Request_time_hour >= 11) & 
                              (uber_data$Request_time_hour <= 16))] <- c("noon")
 
uber_data$day_period[((uber_data$Request_time_hour >= 17) & 
                              (uber_data$Request_time_hour <= 21))] <- c("evening")
 
uber_data$day_period[((uber_data$Request_time_hour >= 22) & 
                              (uber_data$Request_time_hour <= 23))] <- c("night")


write.csv(uber_data,"uber_data.csv", row.names = FALSE)
library(ggplot2)

# plot of numbers of cabs for different status i.e. trip completed, cancelled or no cars available with colours showing different pickup point.
# this plot tells the frequency of request got cancelled, no cars available and trips completed
plot1 <- ggplot(uber_data, aes(x = factor(Status), fill = factor(Pickup.point))) + geom_bar(position = "dodge")+ 
  geom_text(stat = "count", aes(label= ..count..),position = position_dodge(.9), size=4, vjust= 1.5, colour="black")+
  ggtitle("frequency of cabs for different status w.r.t pickup point") +
  labs(x="Status", y="Frequency of Cabs") + labs(fill="Pickup Point")
plot1

#plot of demand of uber cabs on differnt hours with colors showing pickup point either city or airport.
# this plot shows frequency of request for differnt hours. and based on frequency of cabs on hours basis, day period are created.
plot2 <- ggplot(uber_data, aes(x = factor(Request_time_hour), fill = factor(Pickup.point))) + geom_bar(position = "dodge") +  
  ggtitle("Demand for Uber cabs on hours basis") +
  labs(x="Time(Hours)", y="Frequency of Cabs") + labs(fill="Pickup Point")
plot2

# Plot shows demand of uber cabs for differnt day periods sorted in decreasing order with colours showing status.
plot3 <- ggplot(uber_data, aes(x = factor(day_period, levels = names(sort(table(day_period), decreasing=TRUE))), fill = factor(Status))) + geom_bar(position = "stack")+
  ggtitle("Demand for Uber cabs on day period basis w.r.t Status") +
  labs(x="day period", y="Frequency of Cabs")+ labs(fill="Status") +
  geom_text(stat = "count", aes(label= ..count..),size = 3, position = position_stack(vjust = 0.5), colour="black")
plot3

# For plotting demand vs supply of cabs we need to caluculate all this
#Number of trips requested  during early morning = 578
demand_1 <- length(which(uber_data$day_period == "early_morning"))
demand_1

#Number of trips completed during early morning = 214
supply_1 <- length(which((uber_data$day_period == "early_morning")& (uber_data$Status == "trip completed")))
supply_1

#Number of trips requested  during morning = 2346
demand_2 <- length(which(uber_data$day_period == "morning"))
demand_2

#Number of trips completed from city to airport during morning = 970
supply_2 <- length(which((uber_data$day_period == "morning")& (uber_data$Status == "trip completed")))
supply_2

#Number of trips requested  during noon = 981
demand_3 <- length(which(uber_data$day_period == "noon"))
demand_3

#Number of trips completed from city to airport during noon = 606
supply_3 <- length(which((uber_data$day_period == "noon")& (uber_data$Status == "trip completed")))
supply_3

#Number of trips requested  during evening = 2342
demand_4 <- length(which(uber_data$day_period == "evening"))
demand_4

#Number of trips completed from city to airport during evening = 784
supply_4 <- length(which((uber_data$day_period == "evening")& (uber_data$Status == "trip completed")))
supply_4

#Number of trips requested  during night = 498
demand_5 <- length(which(uber_data$day_period == "night"))
demand_5

#Number of trips completed from city to airport during night = 257
supply_5 <- length(which((uber_data$day_period == "night")& (uber_data$Status == "trip completed")))
supply_5

# created data frame to with 3 columns and 5 rows to store the above result of demand and supply
demand_supply <- data.frame(matrix(ncol = 3, nrow = 5))
x <- c("day_period", "demand", "supply")
colnames(demand_supply) <- x
demand_supply$day_period <- c("early morning", "morning", "noon", "evening", "night")
demand_supply$demand <- c(demand_1, demand_2, demand_3, demand_4, demand_5)
demand_supply$supply <- c(supply_1, supply_2, supply_3, supply_4, supply_5)

# this plot is for demand vs supply of cabs.
plot4  <- ggplot(demand_supply)  + 
  geom_bar(aes(x=factor(day_period), y=demand),stat="identity", fill="tan1", colour="sienna3")+
  geom_bar(aes(x=factor(day_period), y=supply),stat="identity")+
  scale_y_continuous(name = "demand", sec.axis = sec_axis(~ . * 1, name = "supply"))+
  geom_text(aes(x = day_period, y = demand, label= demand),size = 3, vjust = 1.5, colour="black")+
  geom_text(aes(x = day_period, y = supply, label= supply),size = 3, vjust = 1.5, colour="black")+
  ggtitle("Demand and supply of uber cabs for different day periods")+
  labs(x="day_period")+
  annotate("text",x=-Inf,y=Inf, label="orange color shows demand while grey colour shows supply", hjust=-.1,vjust=1)
plot4

# As clear from above graph morning day period is problmatic, therefore subset data for morning day period
# Problem 1 = Most number of cab requests got cancelled during the Morning day period
Prob_1 <- subset(uber_data,uber_data$day_period =="morning")

#Total number of trips cancelled in morning = 905
trip_cancel_total <- length(which(Prob_1$Status == "cancelled"))
trip_cancel_total

# Number of trips cancelled from airport in morning = 32
trip_cancel_airport <- length(which((Prob_1$Pickup.point == "airport") & (Prob_1$Status == "cancelled")))
trip_cancel_airport

# Number of trips cancelled from city in morning = 873
trip_cancel_city <- length(which((Prob_1$Pickup.point == "city") & (Prob_1$Status == "cancelled")))
trip_cancel_city

# Percentage of trips cancelled from city in morning = 96.46%
trip_cancel_city_percent <- ((trip_cancel_city / trip_cancel_total) * 100)
trip_cancel_city_percent

# Percentage of trips cancelled from airport in morning = 3.54%
trip_cancel_airport_percent <- ((trip_cancel_airport / trip_cancel_total) * 100)
trip_cancel_airport_percent


# plot of frequrncy of cabs for differnt status during morning with colour showing different pickup point
plot5 <- ggplot(Prob_1,aes(x=factor(Status,levels = names(sort(table(Status), decreasing=TRUE))), fill=factor(Pickup.point))) + geom_bar(stat="count",position = "stack")+
  ggtitle("morning cab Status")+ labs(x="Status",y="Frequency of Cabs")+
  labs(fill="Pickup Point") +
  annotate("text",x=-Inf,y=Inf, label="trip cancel from airport - 3.54% & trip cancel from city = 96.46%", hjust=-.1,vjust=1)
plot5


# Number of trips requested from city to airport during morning = 1845
trip_city_demand <- length(which(Prob_1$Pickup.point == "city"))
trip_city_demand

#Number of trips completed from city to airport during morning = 535
trip_city_supply <- length(which((Prob_1$Pickup.point == "city")& (Prob_1$Status == "trip completed")))
trip_city_supply

# To be more precise and to verify for problem 1, we subset the dataframe for pickup point from city only
pickup_city <- subset(uber_data, uber_data$Pickup.point == "city")
plot6 <- ggplot(pickup_city, aes(x = factor(day_period, levels = names(sort(table(day_period), decreasing=TRUE))), fill = factor(Status))) + geom_bar(position = "stack") +  
  ggtitle("Demand for Uber cabs for pickup from city on day period basis w.r.t Status") +
  labs(x="day period", y="Frequency of Cabs")+ labs(fill="Status") +
  geom_text(stat = "count", aes(label= ..count..),size = 3, position = position_stack(vjust = 0.5), colour="black")
plot6

# Number of trips requested from city to airport during morning = 1845, same answer as above
trip_city_demand_1 <- length(which(pickup_city$day_period == "morning"))
trip_city_demand_1

#Number of trips completed from city to airport during morning = 535, same answer as above
trip_city_supply_1 <- length(which((pickup_city$day_period == "morning")& (pickup_city$Status == "trip completed")))
trip_city_supply_1

# Problem 2 = Most numbers of car not avaialable status during evening day period.

Prob_2 <- subset(subset(uber_data,uber_data$day_period =="evening"))

# Total number of cab request showing no cars avaialble in evening = 1392
no_car_available_total <- length(which(Prob_2$Status == "no cars available"))
no_car_available_total

# Total number of cab requests showing no cars available from airport in evening = 1321
no_car_available_airport <- length(which((Prob_2$Pickup.point == "airport") & (Prob_2$Status == "no cars available")))
no_car_available_airport

# Total number of cab requests showing no cars available from city in evening = 71
no_car_available_city <- length(which((Prob_2$Pickup.point == "city") & (Prob_2$Status == "no cars available")))
no_car_available_city

# Percentage of cab requests showing no cars available from city in evening = 5.10
no_car_available_city_percent <- ((no_car_available_city / no_car_available_total) * 100)
no_car_available_city_percent

# Percentage of cab requests showing no cars available from airport in evening = 94.90
no_car_available_airport_percent <- ((no_car_available_airport / no_car_available_total) * 100)
no_car_available_airport_percent

# plot of frequrncy of cabs for differnt status during evening with colour showing different pickup point
plot7 <- ggplot(Prob_2,aes(x=factor(Status, levels = names(sort(table(Status), decreasing=TRUE))),fill=factor(Pickup.point))) + geom_bar(stat="count",position = "stack")+
  ggtitle("Evening Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point") +
  annotate("text",x=-Inf,y=Inf, label="no car available at airport - 94.90% & no car available at city = 5.10%", hjust=-.1,vjust=1)
plot7

#Total Number of  requests from airport in evening = 1800
demand_nocar_request_airport <- length(which(Prob_2$Pickup.point == "airport"))
demand_nocar_request_airport

#Total Number of  requests from airport got completed in evening = 373
demand_nocar_request_airport_completed <- length(which((Prob_2$Pickup.point == "airport") & (Prob_2$Status == "trip completed")))
demand_nocar_request_airport_completed


# To be more precise and to verify for problem 2, we subset the dataframe for pickup point from airport only
pickup_airport <- subset(uber_data, uber_data$Pickup.point == "airport")

# plot of demand of uber cabs for pickup from airport on day period basis and colour shows different status
plot8 <- ggplot(pickup_airport, aes(x = factor(day_period, levels = names(sort(table(day_period), decreasing=TRUE))), fill = factor(Status))) + geom_bar(position = "stack") +  
  ggtitle("Demand for Uber cabs for pickup from airport on day period basis w.r.t Status") +
  labs(x="day period", y="Frequency of Cabs")+ labs(fill="Status") +
  geom_text(stat = "count", aes(label= ..count..),size = 3, position = position_stack(vjust = 0.5), colour="black")
plot8

#Total Number of  requests from airport in evening = 1800, same as above
demand_nocar_request_airport_1 <- length(which(pickup_airport$day_period == "evening"))
demand_nocar_request_airport_1

#Total Number of  requests from airport got completed in evening = 373
demand_nocar_request_airport_completed_1 <- length(which((pickup_airport$day_period == "evening") & (pickup_airport$Status == "trip completed")))
demand_nocar_request_airport_completed_1









