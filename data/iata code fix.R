
library(readr); library(tidyverse)
FLG <- read_csv("BUS256 - Marketing Analytics/Project/flights.csv")

#https://www.transtats.bts.gov/FieldInfo.asp?Field_Desc=Origin%20Airport%2C%20Airport%20ID.%20An%20identification%20number%20assigned%20by%20US%20DOT%20to%20identify%20a%20unique%20airport.%20%20Use%20this%20field%20for%20airport%20analysis%20across%20a%20range%20of%20years%20because%20an%20airport%20can%20change%20its%20airport%20code%20and%20airport%20codes%20can%20be%20reused.&Field_Type=Num&Lookup_Table=L_AIRPORT_ID&Table_ID=292&SYS_Table_Name=T_T100_MARKET_ALL_CARRIER&Sys_Field_Name=ORIGIN_AIRPORT_ID
L_AIRPORT <- read.csv("C:/Users/mdudu/Desktop/BUS256 - Marketing Analytics/Project/L_AIRPORT.csv")
L_AIRPORT_ID <- read_csv("BUS256 - Marketing Analytics/Project/L_AIRPORT_ID.csv")

FLG <- FLG %>%
  mutate_if(is.character, as.factor)

FLG$MONTH <- as.factor(FLG$MONTH)
FLG$DAY <- as.factor(FLG$DAY)
FLG$DAY_OF_WEEK <- as.factor(FLG$DAY_OF_WEEK)
FLG$DIVERTED <- as.factor(FLG$DIVERTED)
FLG$CANCELLED <- as.factor(FLG$CANCELLED)
FLG$try <- as.numeric(FLG$DESTINATION_AIRPORT)

#filter to october data and clean it up a bit
october <- FLG %>% filter(MONTH == "10")
not_october <- FLG %>% filter(MONTH != "10")

#clean up code a little
october$ORIGIN_AIRPORT <- droplevels(october$ORIGIN_AIRPORT)
october$DESTINATION_AIRPORT <- droplevels(october$DESTINATION_AIRPORT)

#get unique october airport codes
unique_origin_codes <- as.data.frame(unique(october$ORIGIN_AIRPORT))%>% rename(ORIGIN_AIRPORT = `unique(october$ORIGIN_AIRPORT)`)
unique_origin_codes$ORIGIN_AIRPORT <- droplevels(unique_origin_codes$ORIGIN_AIRPORT)

unique_dest_codes <- as.data.frame(unique(october$DESTINATION_AIRPORT))%>%rename(DESTINATION_AIRPORT = `unique(october$DESTINATION_AIRPORT)`)
unique_dest_codes$DESTINATION_AIRPORT <- droplevels(unique_dest_codes$DESTINATION_AIRPORT)

# map 5 letter codes to 3 letter codes
#ORIGIN
airport_origin_fix <- merge(L_AIRPORT, L_AIRPORT_ID, by.x = "Description", by.y = "Description")%>%
  rename(ORIGIN_AIRPORT = Code.y )%>%distinct(ORIGIN_AIRPORT, Code.x) %>% 
  filter(ORIGIN_AIRPORT %in% unique_5_codes$ORIGIN_AIRPORT)%>% mutate_if(is.integer, as.factor)
airport_origin_fix$Code.x <- droplevels(airport_origin_fix$Code.x)

#DESTINATION
airport_dest_fix <- merge(L_AIRPORT, L_AIRPORT_ID, by.x = "Description", by.y = "Description")%>%
  rename(DESTINATION_AIRPORT = Code.y )%>%distinct(DESTINATION_AIRPORT, Code.x)%>% 
  filter(DESTINATION_AIRPORT %in% unique_dest_codes$DESTINATION_AIRPORT)%>%
  mutate_if(is.integer, as.factor)
airport_dest_fix$Code.x <- droplevels(airport_dest_fix$Code.x)

# fix codes in october slide of the flights data
#ORIGIN
october <- october %>% inner_join(airport_origin_fix)%>%
  select(-ORIGIN_AIRPORT)%>% rename(ORIGIN_AIRPORT = Code.x)

#DESTINATION
october <- october1 %>% inner_join(airport_dest_fix)%>%
  select(-DESTINATION_AIRPORT)%>% rename(DESTINATION_AIRPORT = Code.x)

FLG <- rbind(not_october, october) 

