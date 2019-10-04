library(ggplot2)
library(data.table)

flight <- read.csv("flights.csv",colClasses=c("YEAR" = "character", "DEPARTURE_TIME" = "character",
                                              "MONTH" = "character", "WHEELS_OFF" = "character",
                                              "DAY" = "character", "ORIGIN_AIRPORT" = "character",
                                              "SCHEDULED_DEPARTURE" = "character", "WHEELS_ON" = "character",
                                              "SCHEDULED_ARRIVAL" = "character", "ARRIVAL_TIME" = "character"))
flight <- as.data.table(flight)

#Date manipulate(departure_time)
# flight$departure_time <- ISOdatetime(flight$YEAR, flight$MONTH, 
#                                      flight$DAY,as.integer(substr(flight$SCHEDULED_DEPARTURE,1,2)),
#                                      as.integer(substr(flight$SCHEDULED_DEPARTURE,3,4)),0)
flights2 <- flight

#Select col
select_col <- c("MONTH", "DAY", "DAY_OF_WEEK", 
                "AIRLINE", "ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "SCHEDULED_DEPARTURE", 
                "DEPARTURE_TIME", "DEPARTURE_DELAY", "TAXI_OUT", "SCHEDULED_TIME", 
                "ELAPSED_TIME", "AIR_TIME", "DISTANCE", "TAXI_IN", "ARRIVAL_DELAY")
flights2 <- flights2[, select_col, with=F]

# nan
flights2 <- na.omit(flights2, invert = F)

flights2 <- flights2[is.na(as.numeric(flights2$ORIGIN_AIRPORT)),] #numeric airport id. should be deleted.

flights2_dim <- dim(flights2)
print(flights2_dim)

# dummy variable
library(nnet)
month_mat <- class.ind(flights2$MONTH)
colnames(month_mat) <- paste("month_", colnames(month_mat), sep = '')

day_mat <- class.ind(flights2$DAY)
colnames(day_mat) <- paste("day_", colnames(day_mat), sep = '')

day_of_week_mat <- class.ind(flights2$DAY_OF_WEEK)
colnames(day_of_week_mat) <- paste("day_of_week_", colnames(day_of_week_mat), sep = '')

airline_mat <- class.ind(flights2$AIRLINE)
colnames(airline_mat) <- paste("airline_", colnames(airline_mat), sep = '')

# 300 airport, memory is full!!!
# ori_airport_mat <- class.ind(flights2$ORIGIN_AIRPORT)
# colnames(ori_airport_mat) <- paste("origin_airport_", colnames(ori_airport_mat), sep = '')
# 
# des_airport_mat <- class.ind(flights2$DESTINATION_AIRPORT)
# colnames(des_airport_mat) <- paste("des_airport_", colnames(des_airport_mat), sep = '')

## SCHEDULED_DEPARTURE
flights2$SCHEDULED_DEPARTURE <- as.numeric(flights2$SCHEDULED_DEPARTURE)
flights2$is_first_sch_dep <- 0
flights2[between(SCHEDULED_DEPARTURE, 0, 799), is_first_sch_dep := 1 ]

flights2$is_second_sch_dep <- 0
flights2[between(SCHEDULED_DEPARTURE, 800, 1599), is_second_sch_dep := 1]

flights2$is_third_sch_dep <- 0
flights2[between(SCHEDULED_DEPARTURE, 1600, 2359), is_third_sch_dep := 1]

## DEPARTURE_TIME
flights2$DEPARTURE_TIME <- as.numeric(flights2$DEPARTURE_TIME)
flights2$is_first_dep <- 0
flights2[between(DEPARTURE_TIME, 0, 799), is_first_dep := 1]

flights2$is_second_dep <- 0
flights2[between(DEPARTURE_TIME, 800, 1599), is_second_dep := 1]

flights2$is_third_dep <- 0
flights2[between(DEPARTURE_TIME, 1600, 2359), is_third_dep := 1]


flights2 <- cbind(flights2, month_mat, day_mat, day_of_week_mat,
                  airline_mat) 

# flights2 <- cbind(flights2, month_mat, day_mat, day_of_week_mat,
#                   airline_mat, ori_airport_mat, des_airport_mat) 

write.csv(flights2, "flights0930.csv")
