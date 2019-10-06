
#*********************************TABLE OF CONTENTS *************************************#
#1. INTRODUCTION 
#2 DATA SECTION
  #2.1 DESCRIPTION OF DATA 
  #2.2 DATA CLEANING  
    #2.2 (i) DATA TYPES
    #2.2 (ii) LABELS ON MONTH. DAY OF WEEK, ETC
    #2.2 (iii) AIRPORT CODE FIX HERE
    #2.2 (iv) AIRLINES AND AIRPORTS
    #2.2 (v) MSSING DATA
#3 VISUALIZATIONS
  #3.1 ADD SECTIONS HERE 
#4 STATISTICAL ANALYSES
  #4.1 SEASON
  #4.2 REGION
  #4.3 DISTANCE
  #4.4 AIRPORT

#5 REGRESSIONS 

#6 SEGMENTATION 

# CONCLUSION

#*************************************IMPORT ALL DATA HERE*****************************************************#

library(readr); library(tidyverse); library(broom); library(pander)
FLG <- read_csv("BUS256 - Marketing Analytics/Project/flights.csv")
airports <- read_csv("BUS256 - Marketing Analytics/Project/airports.csv")
airlines <- read_csv("BUS256 - Marketing Analytics/Project/airlines.csv")
L_AIRPORT <- read.csv("BUS256 - Marketing Analytics/Project/iata code fix/L_AIRPORT.csv")
L_AIRPORT_ID <- read_csv("BUS256 - Marketing Analytics/Project/iata code fix/L_AIRPORT_ID.csv" )

#********************************INTRODUCTION *******************************************#

#Why are interested in this?

#********************************DESCRIPTION OF DATA ************************************#
#YEAR, MONTH, DAY, DAY_OF_WEEK: refers to date the flight occurred 
#AIRLINE: A code assigned by U.S. Department of Transportation to identify airlines
#ORIGIN_AIRPORT and DESTINATION_AIRPORT: 3-letter code assigned by IATA to uniquely identify the airports
#SCHEDULED_DEPARTURE and SCHEDULED_ARRIVAL : scheduled times of take-off and landing
#DEPARTURE_TIME and ARRIVAL_TIME: actualy take-off and landing times
#DEPARTURE_DELAY and ARRIVAL_DELAY: difference (in minutes) between scheduled and actual times
#DISTANCE: distance (in miles) between airports


#***********************************DATA CLEANING GOES HERE****************************************************#
#rm(list=setdiff(ls(), c("L_AIRPORT_ID", "L_AIRPORT", "airlines","airports", "FLG")))
#***************************#
#(1) SUB-SECTION: DATA TYPES

FLG <- FLG %>% mutate_if(is.character, as.factor)

FLG$DAY <- as.factor(FLG$DAY)
FLG$DIVERTED <- as.factor(FLG$DIVERTED)
FLG$CANCELLED <- as.factor(FLG$CANCELLED)
airports$IATA_CODE <- as.factor(airports$IATA_CODE)

#**************************************************#
#(2) SUB-SECTION: LABELS ON MONTH. DAY OF WEEK, ETC
#Clean Day of Week
FLG <- FLG %>% mutate(DAY_OF_WEEK = ifelse(DAY_OF_WEEK == 1, "Monday", ifelse(DAY_OF_WEEK == 2, "Tuesday",
                                     ifelse(DAY_OF_WEEK == 3, "Wednesday",ifelse(DAY_OF_WEEK == 4, "Thursday",
                                     ifelse(DAY_OF_WEEK == 5, "Friday",ifelse(DAY_OF_WEEK == 6, "Saturday",
                                     ifelse(DAY_OF_WEEK == 7,"Sunday", "Missing" ))))))))

FLG$DAY_OF_WEEK <- as.factor(FLG$DAY_OF_WEEK)


# Clean up distance 

FLG <- FLG %>% mutate(DISTANCEGROUP = ifelse(DISTANCE <250, "<250 Miles", 
                                      ifelse(DISTANCE >=250 & DISTANCE <750 , "250-749 Miles",
                                      ifelse(DISTANCE >= 750 & DISTANCE <1250, "750-1249 Miles",
                                      ifelse(DISTANCE >= 1250 & DISTANCE <1750, "1250-1749 Miles",
                                      ifelse(DISTANCE >= 1750 & DISTANCE <2250, "1750-2249 Miles",
                                      ifelse(DISTANCE >= 2250, "2249+ Miles", "Missing")))))))

#Create Season
FLG <- FLG %>% mutate( SEASON = ifelse(MONTH == 12 | MONTH <=2 , "Winter",
                                  ifelse(MONTH >=9 & MONTH<= 11 , "Fall", 
                                  ifelse(MONTH >=6 & MONTH <=8 ,"Summer",
                                  ifelse(MONTH>=3 & MONTH<=5, "Spring", "Missing")))))
FLG$SEASON <- as.factor(FLG$SEASON)

#Clean Month
FLG <- FLG %>%mutate(MONTH = ifelse(MONTH == 1, "January", ifelse(MONTH == 2, "February",
                              ifelse(MONTH == 3, "March",ifelse(MONTH == 4, "April",
                              ifelse(MONTH == 5, "May",ifelse(MONTH == 6, "June",
                              ifelse(MONTH == 7, "July",ifelse(MONTH == 8, "August",
                              ifelse(MONTH == 9, "September",ifelse(MONTH == 10, "October",
                              ifelse(MONTH == 11, "November", ifelse(MONTH == 12,"December",
                                                                     "Missing" )))))))))))))
FLG$MONTH <- as.factor(FLG$MONTH)

### Break out departure time
FLG$DEPARTURE_TIME <- as.numeric(FLG$DEPARTURE_TIME)
FLG <- FLG %>%mutate(DepatureTime_of_Day = ifelse(DEPARTURE_TIME >= 800 & DEPARTURE_TIME < 1100, "Morning",
                               ifelse(DEPARTURE_TIME >= 1100 & DEPARTURE_TIME < 1400, "Midday",
                               ifelse(DEPARTURE_TIME >= 1400 & DEPARTURE_TIME < 1700, "Afternoon",
                               ifelse(DEPARTURE_TIME >= 1700 & DEPARTURE_TIME < 2100, "Evening",
                               ifelse(DEPARTURE_TIME >= 2100 & DEPARTURE_TIME < 2359, "Night",
                               ifelse(DEPARTURE_TIME >= 0000 & DEPARTURE_TIME < 800, "Early morning","Missing")))))));
FLG$DepatureTime_of_Day <- as.factor(FLG$DepatureTime_of_Day)

### Break out arrival time 
FLG$ARRIVAL_TIME <- as.numeric(FLG$ARRIVAL_TIME)
FLG <- FLG %>%mutate(ArrivalTime_of_Day = ifelse(ARRIVAL_TIME >= 800 & ARRIVAL_TIME < 1100, "Morning",
                                        ifelse(ARRIVAL_TIME >= 1100 & ARRIVAL_TIME < 1400, "Midday",
                                        ifelse(ARRIVAL_TIME >= 1400 & ARRIVAL_TIME < 1700, "Afternoon",
                                        ifelse(ARRIVAL_TIME >= 1700 & ARRIVAL_TIME < 2100, "Evening",
                                        ifelse(ARRIVAL_TIME >= 2100 & ARRIVAL_TIME < 2359, "Night",
                                        ifelse(ARRIVAL_TIME >= 0000 & ARRIVAL_TIME < 800, "Early morning","Missing")))))))

# Break the delay time into several categories
FLG$DEPARTURE_DELAY <- as.numeric(FLG$DEPARTURE_DELAY)
FLG <- FLG %>%mutate(Depdelay_category = ifelse(DEPARTURE_DELAY < 0, "ahead of schedule",
                                         ifelse(DEPARTURE_DELAY >= 0 & DEPARTURE_DELAY < 30, "0 to 30 minutes late",
                                         ifelse(DEPARTURE_DELAY >= 30 & DEPARTURE_DELAY < 60, "30 to 60 minutes late",
                                         ifelse(DEPARTURE_DELAY >= 60 & DEPARTURE_DELAY < 90, "60 to 90 minutes late",
                                         ifelse(DEPARTURE_DELAY >= 90, "more than 90 minutes late", "Missing"))))))

# Break the delay time into several categories
FLG$ARRIVAL_DELAY <- as.numeric(FLG$ARRIVAL_DELAY)
FLG <- FLG %>%mutate(Arrdelay_category = ifelse(ARRIVAL_DELAY < 0, "ahead of schedule",
                                       ifelse(ARRIVAL_DELAY >= 0 & ARRIVAL_DELAY < 30, "0 to 30 minutes late",
                                       ifelse(ARRIVAL_DELAY >= 30 & ARRIVAL_DELAY < 60, "30 to 60 minutes late",
                                       ifelse(ARRIVAL_DELAY >= 60 & ARRIVAL_DELAY < 90, "60 to 90 minutes late",
                                       ifelse(ARRIVAL_DELAY >= 90, "more than 90 minutes late", "Missing"))))))

FLG <- FLG %>% mutate(DIFFERENCE_DELAY = ARRIVAL_DELAY - DEPARTURE_DELAY)
FLG <- FLG %>%mutate(Diffdelay_category = ifelse(DIFFERENCE_DELAY < 0, "ahead of schedule",
                                           ifelse(DIFFERENCE_DELAY >= 0 & DIFFERENCE_DELAY < 30, "0 to 30 minutes late",
                                           ifelse(DIFFERENCE_DELAY >= 30 & DIFFERENCE_DELAY < 60, "30 to 60 minutes late",
                                           ifelse(DIFFERENCE_DELAY >= 60 & DIFFERENCE_DELAY < 90, "60 to 90 minutes late",
                                           ifelse(DIFFERENCE_DELAY >= 90, "more than 90 minutes late", "Missing"))))))

#*************************************#
#(3) SUB-SECTION: AIRPORT CODE FIX HERE

#filter to october data and clean it up a bit
october <- FLG %>% filter(MONTH == "October")
not_october <- FLG %>% filter(MONTH != "October")

#clean up code a little
october$ORIGIN_AIRPORT <- droplevels(october$ORIGIN_AIRPORT)
october$DESTINATION_AIRPORT <- droplevels(october$DESTINATION_AIRPORT)

not_october$ORIGIN_AIRPORT <- droplevels(not_october$ORIGIN_AIRPORT)
not_october$DESTINATION_AIRPORT <- droplevels(not_october$DESTINATION_AIRPORT)

#get unique october airport codes
unique_origin_codes <- as.data.frame(unique(october$ORIGIN_AIRPORT))%>% rename(ORIGIN_AIRPORT = `unique(october$ORIGIN_AIRPORT)`)
unique_origin_codes$ORIGIN_AIRPORT <- droplevels(unique_origin_codes$ORIGIN_AIRPORT)

unique_dest_codes <- as.data.frame(unique(october$DESTINATION_AIRPORT))%>%rename(DESTINATION_AIRPORT = `unique(october$DESTINATION_AIRPORT)`)
unique_dest_codes$DESTINATION_AIRPORT <- droplevels(unique_dest_codes$DESTINATION_AIRPORT)

# map 5 letter codes to 3 letter codes
#ORIGIN
airport_origin_fix <- merge(L_AIRPORT, L_AIRPORT_ID, by.x = "Description", by.y = "Description")%>%
  rename(ORIGIN_AIRPORT = Code.y )%>%distinct(ORIGIN_AIRPORT, Code.x) %>% 
  filter(ORIGIN_AIRPORT %in% unique_origin_codes$ORIGIN_AIRPORT)%>% mutate_if(is.integer, as.factor);

airport_origin_fix$Code.x <- droplevels(airport_origin_fix$Code.x)

#destination
airport_dest_fix <- merge(L_AIRPORT, L_AIRPORT_ID, by.x = "Description", by.y = "Description")%>%
  rename(DESTINATION_AIRPORT = Code.y )%>%distinct(DESTINATION_AIRPORT, Code.x)%>% 
  filter(DESTINATION_AIRPORT %in% unique_dest_codes$DESTINATION_AIRPORT)%>%
  mutate_if(is.integer, as.factor);

airport_dest_fix$Code.x <- droplevels(airport_dest_fix$Code.x)

# fix codes in october slide of the flights data
#origin
october <- october %>% inner_join(airport_origin_fix)%>% select(-ORIGIN_AIRPORT)%>% rename(ORIGIN_AIRPORT = Code.x)

#destination
october <- october %>% inner_join(airport_dest_fix)%>% select(-DESTINATION_AIRPORT)%>% rename(DESTINATION_AIRPORT = Code.x)

FLG <- rbind(not_october, october) 

#remove dataframes
rm(october, not_october, airport_dest_fix, airport_origin_fix, unique_dest_codes, unique_origin_codes,L_AIRPORT, L_AIRPORT_ID)

#*************************************#
#(4) SUB-SECTION: AIRLINES AND AIRPORTS

#(1)merge with flights with  airline 
FLG <- merge(FLG, airlines, by.x = "AIRLINE", by.y = "IATA_CODE") %>% select(-AIRLINE) %>%  
  rename(AIRLINE = AIRLINE.y)
FLG$AIRLINE <- as.factor(FLG$AIRLINE)

#(2) merge with flights with  airport 
## (i) origin airport
FLG <- left_join(FLG, airports[,c("IATA_CODE","AIRPORT", "LATITUDE","LONGITUDE","REGION")] , by = c("ORIGIN_AIRPORT" = "IATA_CODE"))%>% 
  select(-ORIGIN_AIRPORT) %>% rename(ORIGIN_AIRPORT=AIRPORT, ORIGIN_LATITUDE=LATITUDE, ORIGIN_LONGITUDE=LONGITUDE, ORIGIN_REGION=REGION)

FLG$ORIGIN_AIRPORT <- as.factor(FLG$ORIGIN_AIRPORT)

## (ii) destination airport
FLG <- left_join(FLG, airports[, c("IATA_CODE","AIRPORT", "LATITUDE","LONGITUDE","REGION")], by =c("DESTINATION_AIRPORT" = "IATA_CODE")) %>% 
  select(-DESTINATION_AIRPORT) %>% rename(DESTINATION_AIRPORT = AIRPORT, DESTINATION_LATITUDE=LATITUDE, DESTINATION_LONGITUDE=LONGITUDE, DESTINATION_REGION=REGION)

FLG$DESTINATION_AIRPORT <- as.factor(FLG$DESTINATION_AIRPORT)

#remove dataframes
rm(airports, airlines)


#**************************************MISSING DATA*************************************************#
 
#filter to only departure 
FLG_dep <- FLG %>%
  filter(CANCELLED == 0) %>%
  select(-contains("ARRIVAL"), -CANCELLED, -CANCELLATION_REASON, -Arrdelay_category, -DIFFERENCE_DELAY, -Diffdelay_category, -contains("DESTINATION"))%>%
  rename(SCEDULED_TAKEOFF_LANDING = SCHEDULED_DEPARTURE,
         ACTUAL_TAKEOFF_LANDING = DEPARTURE_TIME,
         DELAY = DEPARTURE_DELAY,
         REGION = ORIGIN_REGION, 
         AIRPORT = ORIGIN_AIRPORT, 
         LATITUDE = ORIGIN_LATITUDE,
         LONGITUDE = ORIGIN_LONGITUDE,
         TIME_OF_DAY = DepatureTime_of_Day,
         DELAY_TYPE = Depdelay_category)%>% 
  mutate( ID = "Departure")

#check NAs
anyNA(FLG_dep$DEPARTURE_DELAY) # No NAs in DEPARTURE_DELAY column

# #filter to only arrivals 
FLG_arr <- FLG %>%
  filter(CANCELLED == 0 & DIVERTED ==  0) %>%
  select(-contains("DEPARTURE"), -CANCELLED, -CANCELLATION_REASON,-DIFFERENCE_DELAY,-DepatureTime_of_Day,-Depdelay_category,-Diffdelay_category, -contains("ORIGIN"))%>%
  rename(SCEDULED_TAKEOFF_LANDING = SCHEDULED_ARRIVAL,
         ACTUAL_TAKEOFF_LANDING = ARRIVAL_TIME,
         DELAY = ARRIVAL_DELAY,
         REGION = DESTINATION_REGION, 
         AIRPORT = DESTINATION_AIRPORT, 
         LATITUDE = DESTINATION_LATITUDE,
         LONGITUDE = DESTINATION_LONGITUDE,
         TIME_OF_DAY = ArrivalTime_of_Day,
         DELAY_TYPE = Arrdelay_category)%>%
  mutate( ID = "Arrival")

anyNA(FLG_arr$ARRIVAL_DELAY) # No NAs in ARRIVAL_DELAY column

#merge arrivals and departure dataframes into one 
FLG <- rbind(FLG_dep, FLG_arr)
rm(FLG_dep, FLG_arr)

#**************************************VISUALIZATIONS*************************************************#

# Ran put something here...........


#**************************************STATISTICAL ANALYSES*********************************************#
#*****************************************#
#SUB SECTION (1): T-Tests by season 

#create average delay by season and ID
Season <- FLG %>% group_by(SEASON, ID) %>% summarise(DELAY = mean(DELAY))

#plot season by average delay
ggplot(Season, aes( x = SEASON, y = DELAY, fill = ID)) +
  geom_bar(stat="identity", position="Dodge", alpha=.3, width=.5) + 
  geom_text(aes(label=round(DELAY,2)), position=position_dodge(width = 0.5),family="Times New Roman", size=3, vjust=-.5) +
  labs(fill = "Type", x="Season", title="Average Delay by Seaon", y = "Average Delay (Minutes)")+
  theme_minimal(base_family = "Times New Roman") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#6BAED6","#DE2D26")) + scale_y_continuous(limits = c(NA, 15))

# conduct t-tests for each season
all_year_dep_delay <- FLG %>% filter(ID == "Departure") %>%  select(DELAY)
summer_dep_delay <- FLG %>% filter(SEASON == "Summer" & ID == "Departure")%>% select(DELAY)
winter_dep_delay <- FLG %>% filter(SEASON == "Winter" & ID == "Departure")%>% select(DELAY)
fall_dep_delay <- FLG %>% filter(SEASON == "Fall" & ID == "Departure")%>% select(DELAY)
spring_dep_delay <- FLG %>% filter(SEASON == "Spring" & ID == "Departure")%>% select(DELAY)

t1 <- tidy(t.test(all_year_dep_delay$DELAY,summer_dep_delay$DELAY))[2:5]
#winter vs all year
t2 <- tidy(t.test(all_year_dep_delay$DELAY, winter_dep_delay$DELAY))[2:5]
#fall vs all year
t3 <- tidy(t.test(all_year_dep_delay$DELAY, fall_dep_delay$DELAY))[2:5]
#spring vs all year 
t4 <- tidy(t.test(all_year_dep_delay$DELAY, spring_dep_delay$DELAY))[2:5]

t_season <- data.frame(rbind(t1,t2,t3,t4)) %>%  rename(`Delay for 12months` = estimate1, `Delay by Season` = estimate2)
rownames(t_season) <- c("Summer","Winter","Fall","Spring"); pandoc.table(t_season, style = "grid") 

rm(t1, t2, t3, t4, all_year_dep_delay,summer_dep_delay, winter_dep_delay,fall_dep_delay, spring_dep_delay, Season, t_season)

#*****************************************#
#SUB SECTION (2): T-Tests by Region

#REGION t-tests
REGION <- FLG %>% group_by(REGION, ID) %>% summarise(DELAY = mean(DELAY))

#plot season by average delay
ggplot(REGION, aes( x = REGION, y = DELAY, fill = ID)) +
  geom_bar(stat="identity", position="Dodge", alpha=.3, width=.5) + 
  coord_flip() +
  geom_text(aes(label=round(DELAY,2)), position=position_dodge(width = 0.5),family="Times New Roman", size=3, vjust=-.5) +
  labs(fill = "Type", x="Region", title="Average Delay by Region", y = "Average Delay (Minutes)")+
  theme_minimal(base_family = "Times New Roman") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#6BAED6","#DE2D26")) + scale_y_continuous(limits = c(NA, 15))

all_US_dep_delay <- FLG %>% filter(ID == "Departure") %>%  select(DELAY)
NE_dep_delay <- FLG %>% filter(REGION == "Northeast" & ID == "Departure")%>% select(DELAY)
SOUTH_dep_delay <- FLG %>% filter(REGION == "South"& ID == "Departure")%>% select(DELAY)
MIDWEST_dep_delay <- FLG %>% filter(REGION == "Midwest" & ID == "Departure")%>% select(DELAY)
WEST_dep_delay <- FLG %>% filter(REGION == "West" & ID == "Departure")%>% select(DELAY)
overseas_dep_delay <- FLG %>% filter(REGION == "Over Seas Territory" & ID == "Departure")%>% select(DELAY)

ne <- tidy(t.test(all_US_dep_delay$DELAY, NE_dep_delay$DELAY))[2:5]; sou <- tidy(t.test(all_US_dep_delay$DELAY, SOUTH_dep_delay$DELAY))[2:5] 
mw <- tidy(t.test(all_US_dep_delay$DELAY, MIDWEST_dep_delay$DELAY))[2:5]; we <- tidy(t.test(all_US_dep_delay$DELAY, WEST_dep_delay$DELAY))[2:5] #we vs us  
overseas <- tidy(t.test(all_US_dep_delay$DELAY, overseas_dep_delay$DELAY))[2:5] 

t_region <- data.frame(rbind(ne,sou,mw,we,overseas)) %>% rename(`Average for US` = estimate1, `Average by Region` = estimate2)
rownames(t_region) <- c("NE","South","Midwest","West", "Overseas"); pandoc.table(t_region, style = "grid") 
rm(ne, sou, mw, we, overseas,overseas_dep_delay,WEST_dep_delay,MIDWEST_dep_delay, SOUTH_dep_delay,NE_dep_delay,all_US_dep_delay, REGION,t_region  )

#*****************************************#
#SUB SECTION (3): T-Tests by DISTANCE

#DISTANCEGROUP t-tests
order <- c("<250 Miles",  "250-749 Miles", "750-1249 Miles", "1250-1749 Miles", "1750-2249 Miles","2249+ Miles")
DISTANCEGROUP <- FLG %>% group_by(DISTANCEGROUP, ID) %>% summarise(DELAY = mean(DELAY))%>% ungroup %>%
  mutate(DISTANCEGROUP = factor(DISTANCEGROUP, levels =order))%>% arrange(DISTANCEGROUP)

#plot season by average delay
ggplot(DISTANCEGROUP, aes( x = DISTANCEGROUP, y = DELAY, fill = ID)) +
  geom_bar(stat="identity", position="Dodge", alpha=.3, width=.5) + 
  geom_text(aes(label=round(DELAY,2)), position=position_dodge(width = 0.5),family="Times New Roman", size=3, vjust=-.5) +
  labs(fill = "Type", x="Distance", title="Average Delay by Distance", y = "Average Delay (Minutes)")+
  theme_minimal(base_family = "Times New Roman") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#6BAED6","#DE2D26")) + scale_y_continuous(limits = c(NA, 15)) 

all_dist <- FLG %>% filter(ID == "Departure") %>%  select(DELAY)
less_250 <- FLG %>% filter(DISTANCEGROUP == "<250 Miles" & ID == "Departure")%>% select(DELAY)
d250_750 <- FLG %>% filter(DISTANCEGROUP == "250-749 Miles" & ID == "Departure")%>% select(DELAY)
d750_1250 <- FLG %>% filter(DISTANCEGROUP == "750-1249 Miles" & ID == "Departure")%>% select(DELAY)
d1250_1750 <- FLG %>% filter(DISTANCEGROUP == "1250-1749 Miles" & ID == "Departure")%>% select(DELAY)
d1750_2250 <- FLG %>% filter(DISTANCEGROUP == "1750-2249 Miles" & ID == "Departure")%>% select(DELAY)
d2250plus <- FLG %>% filter(DISTANCEGROUP == "2249+ Miles"& ID == "Departure")%>% select(DELAY)

tless_250 <- tidy(t.test(all_dist$DELAY, less_250$DELAY))[2:5]; t250_750 <- tidy(t.test(all_dist$DELAY, d250_750$DELAY))[2:5] 
t750_1250 <- tidy(t.test(all_dist$DELAY, d750_1250$DELAY))[2:5]; t1250_1750 <- tidy(t.test(all_dist$DELAY, d1250_1750$DELAY))[2:5]  
t1750_2250 <- tidy(t.test(all_dist$DELAY, d1750_2250$DELAY))[2:5];t2250plus <- tidy(t.test(all_dist$DELAY, d2250plus$DELAY))[2:5]  

t_distance <- data.frame(rbind(tless_250, t250_750, t750_1250 ,t1250_1750, t1750_2250, t2250plus)) %>% rename(`all Distances` = estimate1, `Delay by Distance` = estimate2)
rownames(t_distance) <- c("<250 Miles","250-749 Miles","750-1249 Miles","1250-1749 Miles", "1750-2249 Miles", "2249+ Miles"); pandoc.table(t_distance, style = "grid") 
rm(tless_250, t250_750, t750_1250 ,t1250_1750, t1750_2250, t2250plus,less_250, d250_750, d750_1250, order,d1250_1750, d1750_2250, d2250plus, t_distance, all_dist, DISTANCEGROUP)

#*****************************************#
# #SUB SECTION (4): T-Test by AIRPORT SIZE
# Numbers of flights by airport
AIRPORT_ANALYSES <- FLG %>% group_by(AIRPORT, ID) %>% summarise(count = n())%>%
  mutate(AIRPORT_SIZE = ifelse(count < 1000,  "Basic", ifelse(count >= 1000 & count < 10000, 'Local', ifelse(count >= 10000 & count < 40000, 'Regional',
                        ifelse(count >= 40000 & count < 60000, 'Small National', ifelse(count >= 60000 & count < 100000, 'Medium National',
                        ifelse(count >= 100000, 'Large National', 'Missing'))))))) %>%  
  group_by(AIRPORT, AIRPORT_SIZE, ID) %>% summarise(total = sum(count))%>% select(-total);

FLG <- FLG%>%left_join(AIRPORT_ANALYSES, by = c("AIRPORT", "ID")); rm(AIRPORT_ANALYSES)

#AIRPORT t-tests
order <- c('Basic', 'Local',  'Regional', 'Small National','Medium National', 'Large National' )
AIRPORT <- FLG %>% group_by(AIRPORT_SIZE, ID) %>% summarise(DELAY = mean(DELAY))%>% ungroup %>%
  mutate(AIRPORT_SIZE = factor(AIRPORT_SIZE, levels =order))%>% arrange(AIRPORT_SIZE)

#plot season by average delay
ggplot(AIRPORT, aes( x = AIRPORT_SIZE, y = DELAY, fill = ID)) +
  geom_bar(stat="identity", position="Dodge", alpha=.3, width=.5) + 
  geom_text(aes(label=round(DELAY,2)), position=position_dodge(width = 0.5),family="Times New Roman", size=3, vjust=-.5) +
  labs(fill = "Type", x="Airport Size", title="Average Delay by Airport Size", y = "Average Delay (Minutes)")+
  theme_minimal(base_family = "Times New Roman") + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#6BAED6","#DE2D26")) + scale_y_continuous(limits = c(NA, 15)) 

# t-tests 
all_airport <- FLG %>% filter(ID == "Departure") %>%  select(DELAY)
basic <- FLG %>% filter(AIRPORT_SIZE == "Basic" & ID == "Departure")%>% select(DELAY)
local <- FLG %>% filter(AIRPORT_SIZE == "Local" & ID == "Departure")%>% select(DELAY)
reg <- FLG %>% filter(AIRPORT_SIZE == "Regional" & ID == "Departure")%>% select(DELAY)
small_nat <- FLG %>% filter(AIRPORT_SIZE == "Small National" & ID == "Departure")%>% select(DELAY)
medium_nat <- FLG %>% filter(AIRPORT_SIZE == "Medium National" & ID == "Departure")%>% select(DELAY)
large_nat <- FLG %>% filter(AIRPORT_SIZE == "Large National" & ID == "Departure")%>% select(DELAY)

tbasic <- tidy(t.test(all_airport$DELAY, basic$DELAY))[2:5]; tlocal <- tidy(t.test(all_airport$DELAY, local$DELAY))[2:5] 
treg <- tidy(t.test(all_airport$DELAY, reg$DELAY))[2:5]; tsmall_nat <- tidy(t.test(all_airport$DELAY, small_nat$DELAY))[2:5]  
tmedium_nat <- tidy(t.test(all_airport$DELAY, medium_nat$DELAY))[2:5]; tlarge_nat <- tidy(t.test(all_airport$DELAY, large_nat$DELAY))[2:5]  

t_airport <- data.frame(rbind(tbasic, tlocal, treg ,tsmall_nat, tmedium_nat, tlarge_nat)) %>% rename(`all Aiports` = estimate1, `Delay by Size` = estimate2)
rownames(t_airport) <- c('Basic', 'Local',  'Regional', 'Small National','Medium National', 'Large National'); pandoc.table(t_airport, style = "grid") 
rm(tbasic, tlocal, treg ,tsmall_nat, tmedium_nat, tlarge_nat,all_airport, basic, local, order,reg, small_nat, medium_nat, large_nat, t_airport, AIRPORT)

#**************************************REGRESSION ANALYSES*********************************************#
#subsection (1)--> LINEAR

#subsection (2) ---> LOGISTIC 

#**************************************SEGMENTATION ***************************************************#




