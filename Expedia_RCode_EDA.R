setwd("~/Documents/DSO 530 Modeling and Evaluation/Final Project")

all_data=read.csv("all_data5.csv")

library(data.table)
library(ggplot2)
library(dplyr)

#counting missing values
missing_values <- sapply(all_data, function(x) sum(is.na(x)))
missing_values

#counting unique values
unique_values <- sapply(all_data, function(x) length(unique(x)))
unique_values

#stats
summary(all_data)
str(all_data)
dim(all_data)

#correlation between each categorical variable with the target variable
user_country=chisq.test(all_data$user_location_country,all_data$hotel_cluster)
round(user_country$p.value,4)
user_country$p.value
Categorical=c("site_name",'posa_continent', "user_location_country",
                      "user_location_region","user_location_city",
                      "is_mobile","is_package","channel",
                      "srch_destination_id","srch_destination_type_id",
                      "is_booking","hotel_continent","hotel_country",
                      "hotel_market","srch_month","check_in_month")
corr_hotel_cluster=NULL

for(v in Categorical) {
  corr_hotel_cluster=c(corr_hotel_cluster,
                       chisq.test(all_data[,which(names(all_data)==v)],all_data$hotel_cluster)$p.value)
}
corr_hotel_cluster
#all categorical variables are independent with the target variable

#correlation among categorical variables
combinations=combn(Categorical,2)

dim(combinations)

corr_among_categorical=NULL
combinations[1,1]
all_data[,which(names(all_data)==combinations[1,1])]
for(v in 1:120) {
  corr_among_categorical=c(corr_among_categorical,
                       chisq.test(all_data[,which(names(all_data)==combinations[1,v])],
                                  all_data[,which(names(all_data)==combinations[2,v])])$p.value)
}
corr_among_categorical

# #date_time
# hist(as.Date(all_data$date_time),"weeks")
# hist(as.Date(all_data$date_time),"months")
# max(all_data$date_time) #2014-12-31
# min(all_data$date_time) #2013-01-07
# #two years' data from 2013-01-07 to 2014-12-31 
# #concentrating between 2014-03 to 2014-12

#site_name
hist(all_data$site_name)
site_name=all_data %>%
  group_by(site_name) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(site_name))
site_name

ggplot(data=site_name,
       aes(y=count,x=reorder(site_name,count)))+
  xlab("site_name")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#posa_continent
posa_continent=all_data %>%
  group_by(posa_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(posa_continent))
posa_continent

ggplot(data=posa_continent,
       aes(y=count,x=reorder(posa_continent,count)))+
  xlab("posa_continent")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_country
hist(all_data$user_location_country)
user_location_country=all_data %>%
  group_by(user_location_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_country))
user_location_country

ggplot(data=user_location_country[1:50,],
       aes(y=count,x=reorder(user_location_country,count)))+
  xlab("user_location_country")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_region
user_location_region=all_data %>%
  group_by(user_location_region) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_region))
user_location_region

ggplot(data=user_location_region[1:20,],
       aes(y=count,x=reorder(user_location_region,count)))+
  xlab("user_location_region")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#user_location_city
user_location_city=all_data %>%
  group_by(user_location_city) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(user_location_city))
user_location_city

ggplot(data=user_location_city[1:50,],
       aes(y=count,x=reorder(user_location_city,count)))+
  xlab("user_location_city")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#orig_destination_distance
hist(all_data$orig_destination_distance)

#user_id
#may delete this variable

#is_mobile
is_mobile=all_data %>%
  group_by(is_mobile) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_mobile))
is_mobile

ggplot(data=is_mobile,
       aes(y=count,x=reorder(is_mobile,count)))+
  xlab("is_mobile")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#is_package
is_package=all_data %>%
  group_by(is_package) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_package))
is_package

ggplot(data=is_package,
       aes(y=count,x=reorder(is_package,count)))+
  xlab("is_package")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#channel
channel=all_data %>%
  group_by(channel) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(channel))
channel

ggplot(data=channel,
       aes(y=count,x=reorder(channel,count)))+
  xlab("channel")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_ci
hist(all_data$srch_ci,"weeks")
hist(all_data$srch_ci,"months")

srch_ci=all_data %>%
  group_by(srch_ci) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_ci))
srch_ci

ggplot(data=srch_ci[1:20,],
       aes(y=count,x=reorder(srch_ci,count)))+
  xlab("srch_ci")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_co
hist(all_data$srch_co,"weeks")
hist(all_data$srch_co,"months")

boxplot(as.numeric(all_data$srch_co-all_data$srch_ci),xlim=c(0,60))

#srch_adults_cnt
srch_adults_cnt=all_data %>%
  group_by(srch_adults_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_adults_cnt))
srch_adults_cnt

ggplot(data=srch_adults_cnt,
       aes(y=count,x=reorder(srch_adults_cnt,count)))+
  xlab("srch_adults_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_children_cnt
srch_children_cnt=all_data %>%
  group_by(srch_children_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_children_cnt))
srch_children_cnt

ggplot(data=srch_children_cnt,
       aes(y=count,x=reorder(srch_children_cnt,count)))+
  xlab("srch_children_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_rm_cnt
srch_rm_cnt=all_data %>%
  group_by(srch_rm_cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_rm_cnt))
srch_rm_cnt

ggplot(data=srch_rm_cnt,
       aes(y=count,x=reorder(srch_rm_cnt,count)))+
  xlab("srch_rm_cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_destination_id
srch_destination_id=all_data %>%
  group_by(srch_destination_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_destination_id))
srch_destination_id

ggplot(data=srch_destination_id[1:20,],
       aes(y=count,x=reorder(srch_destination_id,count)))+
  xlab("srch_destination_id")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#srch_destination_type_id
srch_destination_type_id=all_data %>%
  group_by(srch_destination_type_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(srch_destination_type_id))
srch_destination_type_id

ggplot(data=srch_destination_type_id,
       aes(y=count,x=reorder(srch_destination_type_id,count)))+
  xlab("srch_destination_type_id")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_continent
hotel_continent=all_data %>%
  group_by(hotel_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_continent))
hotel_continent

ggplot(data=hotel_continent,
       aes(y=count,x=reorder(hotel_continent,count)))+
  xlab("hotel_continent")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_country
hotel_country=all_data %>%
  group_by(hotel_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_country))
hotel_country

ggplot(data=hotel_country[1:20,],
       aes(y=count,x=reorder(hotel_country,count)))+
  xlab("hotel_country")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_market
hotel_market=all_data %>%
  group_by(hotel_market) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_market))
sum(hotel_market[1:15,2])/sum(hotel_market[,2])
#top 15: high freq
sum(hotel_market[1:130,2])/sum(hotel_market[,2])
#15-130: medium freq
#130-rest:low
medium_hotel_market=list(hotel_market[16:130,1])
medium_hotel_market=unlist(medium_hotel_market, use.names=FALSE)
medium_hotel_market[1]
ggplot(data=hotel_market[1:100,],
       aes(y=count,x=reorder(hotel_market,count)))+
  xlab("hotel_market")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#is_booking
is_booking=all_data %>%
  group_by(is_booking) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(is_booking))
is_booking

ggplot(data=is_booking,
       aes(y=count,x=reorder(is_booking,count)))+
  xlab("is booking or not")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#cnt
boxplot(all_data$cnt)
hist(all_data$cnt)

cnt=all_data %>%
  group_by(cnt) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(cnt))
cnt

ggplot(data=cnt[1:20,],
       aes(y=count,x=reorder(cnt,count)))+
  xlab("cnt")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

#hotel_cluster
hist(all_data$hotel_cluster)

hotel_cluster=all_data %>%
  group_by(hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(hotel_cluster))
hotel_cluster

ggplot(data=hotel_cluster[1:20,],
       aes(y=count,x=reorder(hotel_cluster,count)))+
  xlab("hotel_cluster")+
  ylab("count")+
  geom_bar(stat = "identity",fill='#1E90FF')+
  geom_text(aes(label=count),hjust=0)+
  coord_flip()

# Histogram
x=all_data$hotel_cluster
h=hist(all_data$hotel_cluster, breaks=100, col="red", 
       xlab="Hotel Cluster",
       main="Histogram")

#group by country, cluster
country_hotel_cluster=all_data %>%
  group_by(hotel_country,hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

country_hotel_cluster=as.data.frame(country_hotel_cluster)
country_hotel_cluster

#group by orig_destination_distance, cluster
distance_cluster=all_data %>%
  group_by(orig_destination_distance,hotel_cluster) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_cluster=as.data.frame(distance_cluster)
distance_cluster

ggplot(distance_cluster, aes(x=orig_destination_distance,y=count,colour=hotel_cluster))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#the variable is somewhat useful?

#group by orig_destination_distance, srch_destination_type_id
distance_type=all_data %>%
  group_by(orig_destination_distance,srch_destination_type_id) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_type=as.data.frame(distance_type)
distance_type

ggplot(distance_type, aes(x=orig_destination_distance,y=count,colour=srch_destination_type_id))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#the variable is not well separated by srch_destination_type_id

#group by orig_destination_distance, hotel_continent
distance_continent=all_data %>%
  group_by(orig_destination_distance,hotel_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_continent=as.data.frame(distance_continent)
distance_continent

ggplot(distance_continent, aes(x=orig_destination_distance,y=count,colour=hotel_continent))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#a little bit better separated but still not as good

#group by orig_destination_distance, user_location_country
distance_country=all_data %>%
  group_by(orig_destination_distance,user_location_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_country=as.data.frame(distance_country)
distance_country

ggplot(distance_country, aes(x=orig_destination_distance,y=count,colour=user_location_country))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))
#worse than hotel_continent

#group by orig_destination_distance, posa_continent
distance_posa_continent=all_data %>%
  group_by(orig_destination_distance,posa_continent) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_posa_continent=as.data.frame(distance_posa_continent)
distance_posa_continent

ggplot(distance_posa_continent, aes(x=orig_destination_distance,y=count,colour=posa_continent))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))

#group by orig_destination_distance, hotel_country
distance_hotel_country=all_data %>%
  group_by(orig_destination_distance,hotel_country) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  filter(!is.na(orig_destination_distance))

distance_hotel_country=as.data.frame(distance_hotel_country)
distance_hotel_country

ggplot(distance_hotel_country, aes(x=orig_destination_distance,y=count,colour=hotel_country))+ geom_line()+coord_flip()+
  scale_colour_gradientn(colours=rainbow(4))

#study the relationship among variables
all_data$is_booking=as.factor(all_data$is_booking)
ggplot(data = all_data,aes(x = cnt, fill = is_booking)) + 
  geom_bar()+
  xlim(0,15)
