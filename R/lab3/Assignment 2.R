set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")
h_distance <- 300000# These three values are up to the students
h_date <- 10000
h_time <- 20
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times_input <- c("04:00:00", "06:00:00", "24:00:00")
temp <- vector(length = length(times_input))

# Students’ code here
#physical distance from a station to the point of interestS
##
coordinate <- append(st$longitude, st$latitude)
st_coordiante <- matrix(coordinate, ncol = 2)
st_date <- as.Date(st$date)
date <- as.Date(date)

st_time <- as.POSIXct(st$time, format = "%H:%M:%S")
times <- as.POSIXct(times_input, format = "%H:%M:%S")

k_result <- list()
pred_numerator_mult <- c()
pred_denominator_mult <- c()

pred_numerator_sum <- c()
pred_denominator_sum <- c()

pred_temp_mult <- c()
pred_temp_sum <- c()

for (j in 1:length(times)) {
  k_result[[j]] <- matrix(NA, nrow = nrow(st_coordiante), ncol = 7)
  colnames(k_result[[j]]) <- c("Distance", 'k_distance', 'Day_diff',"k_day",'Time_diff', "k_time", 'k_temp')

  for (i in (1:dim(st_coordiante)[1])) {
    k_result[[j]][i, 1] <- distHaversine(st_coordiante[i, ], c(b, a))
    k_result[[j]][i, 2] <- exp(-k_result[[j]][i, 1] ^ 2 / 2 / (h_distance) ^ 2)


    #distance between the day a temperature is measured and the day of interest
    k_result[[j]][i, 3] <- as.numeric(abs(as.Date(st_date[i]) - as.Date(date)))
    k_result[[j]][i, 4] <- exp(-k_result[[j]][i, 3] ^ 2 / 2 / (h_date) ^ 2)

    #distance between the hour of the day temperature is measured and the hour of interest

    k_result[[j]][i, 5] <- as.numeric(abs(difftime(st_time[i], times[j])))
    k_result[[j]][i, 6] <- exp(-k_result[[j]][i, 5] ^ 2 / 2 / (h_time) ^ 2)

    k_result[[j]][i, 7] <- st$air_temperature[i]

  }


  exclude_rows <- rep(FALSE, nrow(k_result[[j]]))

  for (k in (1:nrow(k_result[[j]]))) {
    if (as.numeric(st_date[k] - date) > 0 ||
        ((as.numeric(st_date[k] - date) == 0) &&
         (as.numeric(difftime(
           st_time[k], times[j]
         ))) > 0)) {
      exclude_rows[k] <- TRUE
    }
  }

  k_result[[j]] <- k_result[[j]][!exclude_rows, ]


  pred_numerator_mult[j] <- sum(k_result[[j]][, 2] * k_result[[j]][, 4] * k_result[[j]][, 6] * k_result[[j]][, 7])
  pred_denominator_mult[j] <- sum(k_result[[j]][, 2] * k_result[[j]][, 4] * k_result[[j]][, 6])
  pred_temp_mult[j] <- pred_numerator_mult[j] / pred_denominator_mult[j]

  pred_numerator_sum[j] <- sum((k_result[[j]][, 2] + k_result[[j]][, 4] + k_result[[j]][, 6]) * k_result[[j]][, 7])
  pred_denominator_sum[j] <- sum(k_result[[j]][, 2] + k_result[[j]][, 4] + k_result[[j]][, 6])
  pred_temp_sum[j] <- pred_numerator_sum[j] / pred_denominator_sum[j]

}

library(ggplot2)
k_result_df_mul <- as.data.frame(k_result[[1]])
e <- ggplot(k_result_df_mul, aes(x = Distance, y = k_distance)) +
  geom_line(color = 'red') +
  geom_point()

f <- ggplot(k_result_df_mul, aes(x = Day_diff, y = k_day)) +
  geom_line(color = 'red') +
  geom_point()

g <- ggplot(k_result_df_mul, aes(x = Time_diff, y = k_time)) +
  geom_line(color = 'red') +
  geom_point()


hours <- as.character(substr(times_input, 1, 2))
pred_temp_df <- cbind(hours, pred_temp_mult, pred_temp_sum)

h <- ggplot(pred_temp_df) +
  geom_line(aes(x = hours, y = pred_temp_mult,group=1), color = 'red') +
  geom_point(aes(x = hours, y = pred_temp_mult,group=1), color = 'red') +
  geom_line(aes(x = hours, y = pred_temp_sum,group=2), color = 'blue') +
  geom_point(aes(x = hours, y = pred_temp_sum,group=2), color = 'blue')




print(h)
