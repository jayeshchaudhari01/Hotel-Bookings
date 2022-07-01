#installing packages
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
#importing csv file
hotel_bookings<-read_csv("hotel_bookings.csv")

View(hotel_bookings)
head(hotel_bookings)
glimpse(hotel_bookings)

arrange(hotel_bookings, -lead_time)
min(lead_time) #error
max(hotel_bookings, lead_time) #error


print(max(hotel_bookings$lead_time)) #we must use dollar sign
print(mean(hotel_bookings$lead_time)) #mean of lead_time

print(paste("Maximum lead time: ", max(hotel_bookings$lead_time)))
print(paste("Mean of lead time: ", mean(hotel_bookings$lead_time)))

#grouping 
hotel_bookings %>% 
  group_by(hotel) %>% 
  summarise(average_lead_time=mean(lead_time),
            max_lead_time=max(lead_time),
            min_lead_time=min(lead_time))

#plot for no. of children v/s lead_time to check hypothesis
#Hypothesis:I have a hypothesis that people with children have to book in advance
ggplot(data=hotel_bookings)+
  geom_point(mapping=aes(x=lead_time, y=children))
  

#target audience for weekend_bookings
ggplot(data=hotel_bookings)+
  geom_point(mapping=aes(x=stays_in_weekend_nights, y=children))

#distribution channel graph based on deposit type
ggplot(data=hotel_bookings) +
  geom_bar(mapping=aes(x=distribution_channel, fill=deposit_type))

ggplot(data=hotel_bookings) +
  geom_bar(mapping=aes(x=distribution_channel))+
  facet_grid(~deposit_type)+
  theme(axis.text.x = element_text(angle = 45))

 
#which market segments generate the largest number of bookings
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel, fill = market_segment))+
  facet_wrap(~market_segment)+
  theme(axis.text.x = element_text(angle = 30))


#Your stakeholder asks if you can create a plot that shows the relationship 
#between lead time and guests traveling with children for online bookings at city
#hotels. This will give her a better idea of the specific timing for the promotion.

online_city_hotels<- hotel_bookings %>% 
  filter(market_segment=="Online TA", hotel=="City Hotel")
  

ggplot(data=online_city_hotels)+
  geom_point(mapping=aes(x=lead_time, y=children))
                         
                         
    install.packages("pagedown")
  