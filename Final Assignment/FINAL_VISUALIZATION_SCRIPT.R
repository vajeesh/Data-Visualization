library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(hrbrthemes)
library(tidyverse)

#import the edited excel file in R as dataframe 
Final_df = read_excel("G:/dfonedrives/Final_df.xlsx")                                                                            
Final_df = data.frame(Final_df)
A <- summary(Final_df)

Final_df %>% 
  count(PublicationYear, Region) %>% 
  mutate(perc = n / nrow(Final_df)) -> t2

# Plot 1.Percentage of report recorded over the year and fill by region
jpeg("plot1.png", width = 550, height = 480)
ggplot(t2, aes(PublicationYear, y = perc, fill= Region)) + 
  geom_bar(stat="identity") +
  theme(legend.key.size = unit(0.12, 'cm'))+
  theme(axis.text.x = element_text(angle = 42, vjust = 0.6, hjust=1, size = 7))+
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 8))+
  theme(axis.text.y = element_text(vjust = 0.6, hjust=1, size = 7))+
  theme(axis.title.y = element_text(margin = margin(t = 10), size = 8))+
  xlab("Publication Year")+
  ylab("Reports Recorded in %")+
  scale_y_continuous(labels = percent)
dev.off()

# Plot 2. Plot for the no of reports recorded over the year by region
jpeg("plot3.png", width = 550, height = 480)
ggplot(Final_df, aes(PublicationYear)) + 
  geom_bar(fill = "blue") +  
  facet_wrap(~Region)+
  xlab("Publication Year")+
  ylab("No of Reports Recorded")+
  ggtitle("      Number of reports by Region \n(Retailers sector) ")+
  theme(plot.title = element_text(hjust = 0.5, size = 8))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, hjust=0.9, size = 7))+
  theme(axis.text.y = element_text(vjust = 0.6, hjust=0.9, size = 7))+
  coord_flip()
dev.off() 

# Plot 3. Plot for the no of reports recorded by firm size of all region
jpeg("plot3.png", width = 550, height = 480)
ggplot(Final_df, aes(Size)) + 
  geom_bar(fill = "#DD8888") +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.7, size = 6))+
  facet_wrap(~Region)+
  ggtitle("Number of reports by Region by firm size \n  (Retailers sector) ")+
  theme(plot.title = element_text(hjust = 0.5, size = 8))+
  theme(axis.text.x = element_text(margin = margin(t = 10), size = 8))+
  theme(axis.text.y = element_text(margin = margin(t = 10), size = 8))+
  xlab("Firm Size")+
  ylab("No of Reports")
dev.off() 


Final_df <- read_excel("G:/dfonedrives/Final_df.xlsx")

# Adding the 4 keywords over publication year

pivot <- Final_df%>%
  select(PublicationYear, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(PublicationYear)%>%
  summarise(Diverity_Nr = sum(diversity))

pivot1 <- Final_df%>%
  select(PublicationYear, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(PublicationYear)%>%
  summarise(Greenhouse_Gas_Emmision = sum(greenhouse_gas_emission))

pivot2 <- Final_df%>%
  select(PublicationYear, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(PublicationYear)%>%
  summarise(Customer_Welfare = sum(customer_welfare))

pivot3 <- Final_df%>%
  select(PublicationYear, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(PublicationYear)%>%
  summarise(Employee_Health_and_safety = sum(employee_health_and_safety))

# merging
total<-merge(pivot,pivot1,by = "PublicationYear")
total1<-merge(total,pivot2,by = "PublicationYear")
total2<-merge(total1,pivot3,by = "PublicationYear")

# Removing the 2018 year from the dataframe as only few records recorded and incomplete
total2<- total2[!(total2$PublicationYear ==2018),]      

# Plot 4. Number of words recorded by four different keywords over the years
ggplot(data=total2,mapping = aes(x=PublicationYear))+
  geom_line(mapping = aes(y=Diverity_Nr,group =1, color="Diverity"))+
  geom_point(mapping = aes(y=Diverity_Nr,group =1,color="Diverity"))+
  geom_line(mapping = aes(y=Greenhouse_Gas_Emmision,group =1,color="Greenhouse_Gas_Emmision"))+
  geom_point(mapping = aes(y=Greenhouse_Gas_Emmision,group =1,color="Greenhouse_Gas_Emmision"))+
  geom_line(mapping = aes(y=Customer_Welfare,group =1,color="Customer_Welfare"))+
  geom_point(mapping = aes(y=Customer_Welfare,group =1,color="Customer_Welfare"))+
  geom_line(mapping = aes(y=Employee_Health_and_safety,group =1,color="Employee_Health_and_safety"))+
  geom_point(mapping = aes(y=Employee_Health_and_safety,group =1,color="Employee_Health_and_safety"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Year",y="Number of Years")

# Adding the 4 keywords by Region
pivot <- Final_df%>%
  select(Region, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Region)%>%
  summarise(Diverity_Nr = sum(diversity))

pivot1 <- Final_df%>%
  select(Region, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Region)%>%
  summarise(Greenhouse_Gas_Emmision = sum(greenhouse_gas_emission))

pivot2 <- Final_df%>%
  select(Region, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Region)%>%
  summarise(Customer_Welfare = sum(customer_welfare))

pivot3 <- Final_df%>%
  select(Region, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Region)%>%
  summarise(Employee_Health_and_safety = sum(employee_health_and_safety))

total<-merge(pivot,pivot1,by = "Region")
total1<-merge(total,pivot2,by = "Region")
total2<-merge(total1,pivot3,by = "Region")

# Plot 5. Number of words recorded by four different keywords by six different regions.

ggplot(data=total2,mapping = aes(x=Region))+
  geom_line(mapping = aes(y=Diverity_Nr,group =1, color="Diverity"))+
  geom_point(mapping = aes(y=Diverity_Nr,group =1,color="Diverity"))+
  geom_line(mapping = aes(y=Greenhouse_Gas_Emmision,group =1,color="Greenhouse_Gas_Emmision"))+
  geom_point(mapping = aes(y=Greenhouse_Gas_Emmision,group =1,color="Greenhouse_Gas_Emmision"))+
  geom_line(mapping = aes(y=Customer_Welfare,group =1,color="Customer_Welfare"))+
  geom_point(mapping = aes(y=Customer_Welfare,group =1,color="Customer_Welfare"))+
  geom_line(mapping = aes(y=Employee_Health_and_safety,group =1,color="Employee_Health_and_safety"))+
  geom_point(mapping = aes(y=Employee_Health_and_safety,group =1,color="Employee_Health_and_safety"))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Region",y="No of words")


# Adding the 4 keywords by counties
pivot <- my_data%>%
  select(Country, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Country)%>%
  summarise(Diverity_Nr = sum(diversity))

pivot1 <- my_data%>%
  select(Country, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Country)%>%
  summarise(Greenhouse_Gas_Emmision = sum(greenhouse_gas_emission))

pivot2 <- my_data%>%
  select(Country, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Country)%>%
  summarise(Customer_Welfare = sum(customer_welfare))

pivot3 <- my_data%>%
  select(Country, greenhouse_gas_emission,diversity,employee_health_and_safety,customer_welfare)%>%
  group_by(Country)%>%
  summarise(Employee_Health_and_safety = sum(employee_health_and_safety))

total<-merge(pivot,pivot1,by = "Country")
total1<-merge(total,pivot2,by = "Country")
total2<-merge(total1,pivot3,by = "Country")
View(total2)


TOTAL10<- total2%>%
  rowwise()%>%
  mutate(sumrow = sum(c_across(where(is.numeric))))
TOTAL10
View(TOTAL10)
colnames(TOTAL10)[colnames(TOTAL10)=="Country"]<-"region"

mapdata<- map_data("world")

# merging the two dataframe by the region
mapdata <- left_join(mapdata,TOTAL10, by = "region")
view(mapdata)
# filtering the data and removing the non entry values
mapdata1<-mapdata%>%filter(!is.na(mapdata$sumrow))
View(mapdata1)
mapdata[is.na(mapdata)] <- 0

#plot 6. Number of total keywords recorded by all countries
map1 <-ggplot(mapdata,aes(x=long, y=lat,group=group))+
  geom_polygon(aes(fill= sumrow),color = "black")+
  theme_bw()
map1

map2 <-map1+scale_fill_gradient(name="Number of indicators found", low = "white",high = "green")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        rect = element_blank())
map2


