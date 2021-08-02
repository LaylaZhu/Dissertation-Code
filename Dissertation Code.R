##Preparation
setwd("C:/Users/Administrator/Desktop")
getwd()

library(ggplot2)
library(dplyr)
library(ggpubr)
library(forcats)

##Read the raw data
data <- read.csv("hotel_bookings.csv")
summary(data)
colSums(is.na(data))

##Solve the data issue
data$children[is.na(data$children)]<-mean(data$children,na.rm = TRUE)

##Description of booking changes
ggplot(data, aes(booking_changes)) + 
  geom_histogram(aes(y = ..density..), fill = "steelblue") +
  geom_density(aes(booking_changes), lwd=1, colour = "black") +
  ggtitle("Plot of Booking Changes") + 
  theme(plot.title = element_text(size = 11)) +
  geom_vline(aes(xintercept = mean(booking_changes)), linetype = 5, col = "red")

##Booking changes separated by location 
data %>% count(hotel)
planes1 <- group_by(data, hotel)
delay1 <- summarise(planes1,
                    mean_changes = mean(booking_changes))
delay1
qplot(x = delay1[["hotel"]], y = delay1[["mean_changes"]]) +
  labs(x = "Location", y = "Mean Booking Changes", title = "Booking Changes Separated by Location") +
  geom_bar(stat = "identity", fill = "cornflowerblue")

##Booking changes of different lead time
ggplot(data = data, aes(y = booking_changes, x = lead_time)) +
  geom_point(aes(colour = lead_time)) +
  geom_smooth(method = "lm",  formula = y~x, mapping = aes(y = booking_changes, x = lead_time)) +
  labs(x = "Lead Time", y = "Booking Changes", title = "Booking Changes of Different Lead Time")

##Booking changes separated by arrival month
data %>% count(arrival_date_month)
planes2 <- group_by(data, arrival_date_month)
delay2 <- summarise(planes2,
                    mean_changes = mean(booking_changes))
delay2
qplot(x = reorder(delay2[["arrival_date_month"]], delay2[["mean_changes"]]), y = delay2[["mean_changes"]]) +
  labs(x = "Month", y = "Mean Booking Changes", title = "Booking Changes Separated by Month") +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  theme(axis.text.x = element_text(angle = 20)) 

##Booking changes of different deposit types
data %>% count(deposit_type)
planes3 <- group_by(data, deposit_type)
delay3 <- summarise(planes3,
                    mean_changes = mean(booking_changes))
delay3
qplot(x = reorder(delay3[["deposit_type"]], delay3[["mean_changes"]]), y = delay3[["mean_changes"]]) +
  labs(x = "Deposit Type", y = "Mean Booking Changes", title = "Booking Changes Separated by Deposit Types") +
  geom_bar(stat = "identity", fill = "cornflowerblue")

##Description of Cancellations
data$is_canceled <- as.factor(data$is_canceled)
prop.Cancellation1 <- round(sum(data$is_canceled == "1")/length(data$is_canceled)*100)
prop.Cancellation2 <- round(sum(data$is_canceled == "0")/length(data$is_canceled)*100)
Cancellation <- data.frame(group = c("no", "yes"),
                    value = c(prop.Cancellation2, prop.Cancellation1))
labs <- paste0(Cancellation$group, "(",Cancellation$value, "%)")
ggdonutchart(Cancellation, "value", 
             lable = labs,
             fill = "group", 
             color = "white",
             palette = c("#00AFBB", "#E7B800"),
             title = "Description of Cancellations")

##Cancellations Separated by Location
ggplot(data = data, aes(x = hotel, fill = is_canceled)) +
  geom_density(alpha = 0.5) +
  labs(title = "Cancellations Separated by Location")

##Cancellation Separated by Lead Time
ggplot(data, aes(x = lead_time, fill = is_canceled)) +
  geom_density(alpha = 0.5) +
  labs(title = "Cancellation Separated by Lead Time")

##Cancellations Separated by Arrival Month
ggplot(data = data)+
  geom_bar(mapping = aes(x = arrival_date_month, y = ..count.., fill = is_canceled),position = "dodge")+
  labs(title="Cancellations Separated by Arrival Month",x="Month",y="Cancellations")+
  theme(axis.text.x = element_text(angle = 45))

##Cancellations Separated by Deposit Types
ggplot(data = data)+
  geom_bar(mapping = aes(x = deposit_type, y = ..count.., fill = is_canceled),position = "dodge")+
  labs(title="Cancellations Separated by Deposit Types",x="Deposit Type",y="Cancellations")









